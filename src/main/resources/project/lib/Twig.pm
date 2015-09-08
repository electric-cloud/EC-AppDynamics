use strict;
use warnings; # > perl 5.5

# This is created in the caller's space
# I realize (now!) that it's not clean, but it's been there for 10+ years...
BEGIN
{ sub ::PCDATA { '#PCDATA' }  ## no critic (Subroutines::ProhibitNestedSubs);
  sub ::CDATA  { '#CDATA'  }  ## no critic (Subroutines::ProhibitNestedSubs);
}

use UNIVERSAL();

## if a sub returns a scalar, it better not bloody disappear in list context
## no critic (Subroutines::ProhibitExplicitReturnUndef);

my $perl_version;
my $parser_version;

######################################################################
package XML::Twig;
######################################################################

require 5.004;

use utf8; # > perl 5.5

use vars qw($VERSION @ISA %valid_option);

use Carp;
use File::Spec;
use File::Basename;

*isa= *UNIVERSAL::isa;

# flag, set to true if the weaken sub is available
use vars qw( $weakrefs);

# flag set to true if the version of expat seems to be 1.95.2, which has annoying bugs
# wrt doctype handling. This is global for performance reasons. 
my $expat_1_95_2=0;

# xml name (leading # allowed)
# first line is for perl 5.005, second line for modern perl, that accept character classes
my $REG_NAME       = q{(?:(?:[^\W\d]|[:#_])(?:[\w.-]*:)?[\w.-]*)};     # does not work for leading non-ascii letters
   $REG_NAME       = q{(?:(?:[[:alpha:]:#_])(?:[\w.-]*:)?[\w.-]*)};    # > perl 5.5

# name or wildcard (* or '') (leading # allowed)
my $REG_NAME_W     = q{(?:(?:[^\W\d]|[:#_])(?:[\w.-]*:)?[\w.-]*|\*)}; # does not work for leading non-ascii letters
   $REG_NAME_W     = q{(?:(?:[[:alpha:]:#_])(?:[\w.-]*:)?[\w.-]*|\*)}; # > perl 5.5

# name or wildcard (* or '') (leading # allowed) with optional class
my $REG_NAME_WC    = q{(?(?:(?:[^\W\d]|[:#_])(?:[\w.-]*:)?[\w.-]*|\*)(?:\.[\w-]+)?|(?:\.[\w-]+))}; # does not work for leading non-ascii letters
   $REG_NAME_WC    = q{(?:(?:(?:[[:alpha:]:#_])(?:[\w.-]*:)?[\w.-]*|\*)(?:\.[\w-]+)?|(?:\.[\w-]+))}; # > perl 5.5


my $REG_REGEXP     = q{(?:/(?:[^\\/]|\\.)*/[eimsox]*)};               # regexp
my $REG_MATCH      = q{[!=]~};                                        # match (or not)
my $REG_STRING     = q{(?:"(?:[^\\"]|\\.)*"|'(?:[^\\']|\\.)*')};      # string (simple or double quoted)
my $REG_NUMBER     = q{(?:\d+(?:\.\d*)?|\.\d+)};                      # number
my $REG_VALUE      = qq{(?:$REG_STRING|$REG_NUMBER)};                 # value
my $REG_OP         = q{==|!=|>|<|>=|<=|eq|ne|lt|gt|le|ge|=};          # op
my $REG_FUNCTION   = q{(?:string|text)\(\s*\)};
my $REG_STRING_ARG = qq{(?:string|text)\\(\\s*$REG_NAME_W\\s*\\)};
my $REG_COMP       = q{(?:>=|<=|!=|<|>|=)};

my $REG_TAG_IN_PREDICATE= $REG_NAME . q{(?=\s*(?i:and\b|or\b|\]|$))};


# used in the handler trigger code
my $REG_NAKED_PREDICATE= qq{((?:"[^"]*"|'[^']*'|$REG_STRING_ARG|$REG_FUNCTION|\@$REG_NAME_W|$REG_MATCH\\s*$REG_REGEXP|[\\s\\d><=!()+.-]|(?i:and)|(?i:or)|$REG_TAG_IN_PREDICATE)*)};
my $REG_PREDICATE= qq{\\[$REG_NAKED_PREDICATE\\]};

# not all axis, only supported ones (in get_xpath)
my @supported_axis= ( 'ancestor', 'ancestor-or-self', 'child', 'descendant', 'descendant-or-self', 
                      'following', 'following-sibling', 'parent', 'preceding', 'preceding-sibling', 'self'
                    );
my $REG_AXIS       = "(?:" . join( '|', @supported_axis) .")";

# only used in the "xpath"engine (for get_xpath/findnodes) for now
my $REG_PREDICATE_ALT  = qr{\[(?:(?:string\(\s*\)|\@$REG_NAME)\s*$REG_MATCH\s*$REG_REGEXP\s*|[^\]]*)\]};

# used to convert XPath tests on strings to the perl equivalent 
my %PERL_ALPHA_TEST= ( '=' => ' eq ', '!=' => ' ne ', '>' => ' gt ', '>=' => ' ge ', '<' => ' lt ', '<=' => ' le ');

my( $FB_HTMLCREF, $FB_XMLCREF);

my $NO_WARNINGS= $perl_version >= 5.006 ? 'no warnings' : 'local $^W=0';

# default namespaces, both ways
my %DEFAULT_NS= ( xml   => "http://www.w3.org/XML/1998/namespace",
                  xmlns => "http://www.w3.org/2000/xmlns/",
                );
my %DEFAULT_URI2NS= map { $DEFAULT_NS{$_} => $_ } keys %DEFAULT_NS;

# constants
my( $PCDATA, $CDATA, $PI, $COMMENT, $ENT, $ELT, $TEXT, $ASIS, $EMPTY, $BUFSIZE);

# used when an HTML doc only has a PUBLIC declaration, to generate the SYSTEM one
# this should really be done by HTML::TreeBuilder, but as of HTML::TreeBuilder 4.2 it isn't
# the various declarations are taken from http://en.wikipedia.org/wiki/Document_Type_Declaration
my %HTML_DECL= ( "-//W3C//DTD HTML 4.0 Transitional//EN"  => "http://www.w3.org/TR/REC-html40/loose.dtd",
                 "-//W3C//DTD HTML 4.01//EN"              => "http://www.w3.org/TR/html4/strict.dtd",
                 "-//W3C//DTD HTML 4.01 Transitional//EN" => "http://www.w3.org/TR/html4/loose.dtd",
                 "-//W3C//DTD HTML 4.01 Frameset//EN"     => "http://www.w3.org/TR/html4/frameset.dtd",
                 "-//W3C//DTD XHTML 1.0 Strict//EN"       => "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd",
                 "-//W3C//DTD XHTML 1.0 Transitional//EN" => "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd",
                 "-//W3C//DTD XHTML 1.0 Frameset//EN"     => "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd",
                 "-//W3C//DTD XHTML 1.1//EN"              => "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd",
                 "-//W3C//DTD XHTML Basic 1.0//EN"        => "http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd",
                 "-//W3C//DTD XHTML Basic 1.1//EN"        => "http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd",
                 "-//WAPFORUM//DTD XHTML Mobile 1.0//EN"  => "http://www.wapforum.org/DTD/xhtml-mobile10.dtd",
                 "-//WAPFORUM//DTD XHTML Mobile 1.1//EN"  => "http://www.openmobilealliance.org/tech/DTD/xhtml-mobile11.dtd",
                 "-//WAPFORUM//DTD XHTML Mobile 1.2//EN"  => "http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd",
                 "-//W3C//DTD XHTML+RDFa 1.0//EN"         => "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd",
               );

my $DEFAULT_HTML_TYPE= "-//W3C//DTD HTML 4.0 Transitional//EN";

my $SEP= qr/\s*(?:$|\|)/;

BEGIN
{ 
$VERSION = '3.40';

use XML::Parser;
my $needVersion = '2.23';
($parser_version= $XML::Parser::VERSION)=~ s{_\d+}{}; # remove _<n> from version so numeric tests do not warn
croak "need at least XML::Parser version $needVersion" unless $parser_version >= $needVersion;

($perl_version= $])=~ s{_\d+}{};

if( $perl_version >= 5.008) 
  { eval "use Encode qw( :all)";
    $FB_XMLCREF  = 0x0400; # Encode::FB_XMLCREF;
    $FB_HTMLCREF = 0x0200; # Encode::FB_HTMLCREF;
  }

# test whether we can use weak references
# set local empty signal handler to trap error messages
{ local $SIG{__DIE__};
  if( eval( 'require Scalar::Util') && defined( \&Scalar::Util::weaken)) 
    { import Scalar::Util( 'weaken'); $weakrefs= 1; }
  elsif( eval( 'require WeakRef')) 
    { import WeakRef; $weakrefs= 1;                 }
  else  
    { $weakrefs= 0;                                 } 
}

import XML::Twig::Elt;
import XML::Twig::Entity;
import XML::Twig::Entity_list;

# used to store the gi's
# should be set for each twig really, at least when there are several
# the init ensures that special gi's are always the same

# constants: element types
$PCDATA  = '#PCDATA';
$CDATA   = '#CDATA';
$PI      = '#PI';
$COMMENT = '#COMMENT';
$ENT     = '#ENT';

# element classes
$ELT     = '#ELT';
$TEXT    = '#TEXT';

# element properties
$ASIS    = '#ASIS';
$EMPTY   = '#EMPTY';

# used in parseurl to set the buffer size to the same size as in XML::Parser::Expat
$BUFSIZE = 32768;


# gi => index
%XML::Twig::gi2index=( '', 0, $PCDATA => 1, $CDATA => 2, $PI => 3, $COMMENT => 4, $ENT => 5); 
# list of gi's
@XML::Twig::index2gi=( '', $PCDATA, $CDATA, $PI, $COMMENT, $ENT);

# gi's under this value are special 
$XML::Twig::SPECIAL_GI= @XML::Twig::index2gi;

%XML::Twig::base_ent= ( '>' => '&gt;', '<' => '&lt;', '&' => '&amp;', "'" => '&apos;', '"' => '&quot;',);

# now set some aliases
*find_nodes           = *get_xpath;               # same as XML::XPath
*findnodes            = *get_xpath;               # same as XML::LibXML
*getElementsByTagName = *descendants;
*descendants_or_self  = *descendants;             # valid in XML::Twig, not in XML::Twig::Elt
*find_by_tag_name     = *descendants;
*getElementById       = *elt_id;
*getEltById           = *elt_id;
*toString             = *sprint;
*create_accessors     = *att_accessors;

}

@ISA = qw(XML::Parser);

# fake gi's used in twig_handlers and start_tag_handlers
my $ALL    = '_all_';     # the associated function is always called
my $DEFAULT= '_default_'; # the function is called if no other handler has been

# some defaults
my $COMMENTS_DEFAULT= 'keep';
my $PI_DEFAULT      = 'keep';


# handlers used in regular mode
my %twig_handlers=( Start      => \&_twig_start, 
                    End        => \&_twig_end, 
                    Char       => \&_twig_char, 
                    Entity     => \&_twig_entity, 
                    XMLDecl    => \&_twig_xmldecl, 
                    Doctype    => \&_twig_doctype, 
                    Element    => \&_twig_element, 
                    Attlist    => \&_twig_attlist, 
                    CdataStart => \&_twig_cdatastart, 
                    CdataEnd   => \&_twig_cdataend, 
                    Proc       => \&_twig_pi,
                    Comment    => \&_twig_comment,
                    Default    => \&_twig_default,
                    ExternEnt  => \&_twig_extern_ent,
      );

# handlers used when twig_roots is used and we are outside of the roots
my %twig_handlers_roots=
  ( Start      => \&_twig_start_check_roots, 
    End        => \&_twig_end_check_roots, 
    Doctype    => \&_twig_doctype, 
    Char       => undef, Entity     => undef, XMLDecl    => \&_twig_xmldecl, 
    Element    => undef, Attlist    => undef, CdataStart => undef, 
    CdataEnd   => undef, Proc       => undef, Comment    => undef, 
    Proc       => \&_twig_pi_check_roots,
    Default    =>  sub {}, # hack needed for XML::Parser 2.27
    ExternEnt  => \&_twig_extern_ent,
  );

# handlers used when twig_roots and print_outside_roots are used and we are
# outside of the roots
my %twig_handlers_roots_print_2_30=
  ( Start      => \&_twig_start_check_roots, 
    End        => \&_twig_end_check_roots, 
    Char       => \&_twig_print, 
    Entity     => \&_twig_print_entity, 
    ExternEnt  => \&_twig_print_entity,
    DoctypeFin => \&_twig_doctype_fin_print,
    XMLDecl    => sub { _twig_xmldecl( @_); _twig_print( @_); },
    Doctype   =>  \&_twig_print_doctype, # because recognized_string is broken here
    # Element    => \&_twig_print, Attlist    => \&_twig_print, 
    CdataStart => \&_twig_print, CdataEnd   => \&_twig_print, 
    Proc       => \&_twig_pi_check_roots, Comment    => \&_twig_print, 
    Default    => \&_twig_print_check_doctype,
    ExternEnt  => \&_twig_extern_ent,
  );

# handlers used when twig_roots, print_outside_roots and keep_encoding are used
# and we are outside of the roots
my %twig_handlers_roots_print_original_2_30=
  ( Start      => \&_twig_start_check_roots, 
    End        => \&_twig_end_check_roots, 
    Char       => \&_twig_print_original, 
    # I have no idea why I should not be using this handler!
    Entity     => \&_twig_print_entity, 
    ExternEnt  => \&_twig_print_entity,
    DoctypeFin => \&_twig_doctype_fin_print,
    XMLDecl    => sub { _twig_xmldecl( @_); _twig_print_original( @_) }, 
    Doctype    => \&_twig_print_original_doctype,  # because original_string is broken here
    Element    => \&_twig_print_original, Attlist   => \&_twig_print_original,
    CdataStart => \&_twig_print_original, CdataEnd  => \&_twig_print_original,
    Proc       => \&_twig_pi_check_roots, Comment   => \&_twig_print_original,
    Default    => \&_twig_print_original_check_doctype, 
  );

# handlers used when twig_roots and print_outside_roots are used and we are
# outside of the roots
my %twig_handlers_roots_print_2_27=
  ( Start      => \&_twig_start_check_roots, 
    End        => \&_twig_end_check_roots, 
    Char       => \&_twig_print, 
    # if the Entity handler is set then it prints the entity declaration
    # before the entire internal subset (including the declaration!) is output
    Entity     => sub {},
    XMLDecl    => \&_twig_print, Doctype    => \&_twig_print, 
    CdataStart => \&_twig_print, CdataEnd   => \&_twig_print, 
    Proc       => \&_twig_pi_check_roots, Comment    => \&_twig_print, 
    Default    => \&_twig_print, 
    ExternEnt  => \&_twig_extern_ent,
  );

# handlers used when twig_roots, print_outside_roots and keep_encoding are used
# and we are outside of the roots
my %twig_handlers_roots_print_original_2_27=
  ( Start      => \&_twig_start_check_roots, 
    End        => \&_twig_end_check_roots, 
    Char       => \&_twig_print_original, 
    # for some reason original_string is wrong here 
    # this can be a problem if the doctype includes non ascii characters
    XMLDecl    => \&_twig_print, Doctype    => \&_twig_print,
    # if the Entity handler is set then it prints the entity declaration
    # before the entire internal subset (including the declaration!) is output
    Entity     => sub {}, 
    #Element    => undef, Attlist   => undef,
    CdataStart => \&_twig_print_original, CdataEnd  => \&_twig_print_original,
    Proc       => \&_twig_pi_check_roots, Comment   => \&_twig_print_original,
    Default    => \&_twig_print, #  _twig_print_original does not work
    ExternEnt  => \&_twig_extern_ent,
  );


my %twig_handlers_roots_print= $parser_version > 2.27 
                               ? %twig_handlers_roots_print_2_30 
                               : %twig_handlers_roots_print_2_27; 
my %twig_handlers_roots_print_original= $parser_version > 2.27 
                               ? %twig_handlers_roots_print_original_2_30 
                               : %twig_handlers_roots_print_original_2_27; 


# handlers used when the finish_print method has been called
my %twig_handlers_finish_print=
  ( Start      => \&_twig_print, 
    End        => \&_twig_print, Char       => \&_twig_print, 
    Entity     => \&_twig_print, XMLDecl    => \&_twig_print, 
    Doctype    => \&_twig_print, Element    => \&_twig_print, 
    Attlist    => \&_twig_print, CdataStart => \&_twig_print, 
    CdataEnd   => \&_twig_print, Proc       => \&_twig_print, 
    Comment    => \&_twig_print, Default    => \&_twig_print, 
    ExternEnt  => \&_twig_extern_ent,
  );

# handlers used when the finish_print method has been called and the keep_encoding
# option is used
my %twig_handlers_finish_print_original=
  ( Start      => \&_twig_print_original, End      => \&_twig_print_end_original, 
    Char       => \&_twig_print_original, Entity   => \&_twig_print_original, 
    XMLDecl    => \&_twig_print_original, Doctype  => \&_twig_print_original, 
    Element    => \&_twig_print_original, Attlist  => \&_twig_print_original, 
    CdataStart => \&_twig_print_original, CdataEnd => \&_twig_print_original, 
    Proc       => \&_twig_print_original, Comment  => \&_twig_print_original, 
    Default    => \&_twig_print_original, 
  );

# handlers used within ignored elements
my %twig_handlers_ignore=
  ( Start      => \&_twig_ignore_start, 
    End        => \&_twig_ignore_end, 
    Char       => undef, Entity     => undef, XMLDecl    => undef, 
    Doctype    => undef, Element    => undef, Attlist    => undef, 
    CdataStart => undef, CdataEnd   => undef, Proc       => undef, 
    Comment    => undef, Default    => undef,
    ExternEnt  => undef,
  );


# those handlers are only used if the entities are NOT to be expanded
my %twig_noexpand_handlers= ( ExternEnt => undef, Default => \&_twig_default );

my @saved_default_handler;

my $ID= 'id';  # default value, set by the Id argument
my $css_sel=0; # set through the css_sel option to allow .class selectors in triggers 

# all allowed options
%valid_option=
    ( # XML::Twig options
      TwigHandlers          => 1, Id                    => 1,
      TwigRoots             => 1, TwigPrintOutsideRoots => 1,
      StartTagHandlers      => 1, EndTagHandlers        => 1,
      ForceEndTagHandlersUsage => 1,
      DoNotChainHandlers    => 1,
      IgnoreElts            => 1,
      Index                 => 1, 
      AttAccessors          => 1,
      EltAccessors          => 1,
      FieldAccessors        => 1,
      CharHandler           => 1, 
      TopDownHandlers       => 1,
      KeepEncoding          => 1, DoNotEscapeAmpInAtts  => 1,
      ParseStartTag         => 1, KeepAttsOrder         => 1,
      LoadDTD               => 1, DTDHandler            => 1,
      DoNotOutputDTD        => 1, NoProlog              => 1,
      ExpandExternalEnts    => 1,
      DiscardSpaces         => 1, KeepSpaces            => 1, DiscardAllSpaces => 1,
      DiscardSpacesIn       => 1, KeepSpacesIn          => 1, 
      PrettyPrint           => 1, EmptyTags             => 1, 
      EscapeGt              => 1,
      Quote                 => 1,
      Comments              => 1, Pi                    => 1, 
      OutputFilter          => 1, InputFilter           => 1,
      OutputTextFilter      => 1, 
      OutputEncoding        => 1, 
      RemoveCdata           => 1,
      EltClass              => 1,
      MapXmlns              => 1, KeepOriginalPrefix    => 1,
      SkipMissingEnts       => 1,
      # XML::Parser options
      ErrorContext          => 1, ProtocolEncoding      => 1,
      Namespaces            => 1, NoExpand              => 1,
      Stream_Delimiter      => 1, ParseParamEnt         => 1,
      NoLWP                 => 1, Non_Expat_Options     => 1,
      Xmlns                 => 1, CssSel                => 1,
      UseTidy               => 1, TidyOptions           => 1,
      OutputHtmlDoctype     => 1,
    );

my $active_twig; # last active twig,for XML::Twig::s

# predefined input and output filters
use vars qw( %filter);
%filter= ( html       => \&html_encode,
           safe       => \&safe_encode,
           safe_hex   => \&safe_encode_hex,
         );


# trigger types (used to sort them)
my ($LEVEL_TRIGGER, $REGEXP_TRIGGER, $XPATH_TRIGGER)=(1..3);

sub new
  { my ($class, %args) = @_;
    my $handlers;

    # change all nice_perlish_names into nicePerlishNames
    %args= _normalize_args( %args);

    # check options
    unless( $args{MoreOptions})
      { foreach my $arg (keys %args)
        { carp "invalid option $arg" unless $valid_option{$arg}; }
      }
     
    # a twig is really an XML::Parser
    # my $self= XML::Parser->new(%args);
    my $self;
    $self= XML::Parser->new(%args);   

    bless $self, $class;

    $self->{_twig_context_stack}= [];

    # allow tag.class selectors in handler triggers
    $css_sel= $args{CssSel} || 0; 


    if( exists $args{TwigHandlers})
      { $handlers= $args{TwigHandlers};
        $self->setTwigHandlers( $handlers);
        delete $args{TwigHandlers};
      }

    # take care of twig-specific arguments
    if( exists $args{StartTagHandlers})
      { $self->setStartTagHandlers( $args{StartTagHandlers});
        delete $args{StartTagHandlers};
      }

    if( exists $args{DoNotChainHandlers})
      { $self->{twig_do_not_chain_handlers}=  $args{DoNotChainHandlers}; }

    if( exists $args{IgnoreElts})
      { # change array to hash so you can write ignore_elts => [ qw(foo bar baz)]
        if( isa( $args{IgnoreElts}, 'ARRAY')) { $args{IgnoreElts}= { map { $_ => 1 } @{$args{IgnoreElts}} }; }
        $self->setIgnoreEltsHandlers( $args{IgnoreElts});
        delete $args{IgnoreElts};
      }

    if( exists $args{Index})
      { my $index= $args{Index};
        # we really want a hash name => path, we turn an array into a hash if necessary
        if( ref( $index) eq 'ARRAY')
          { my %index= map { $_ => $_ } @$index;
            $index= \%index;
          }
        while( my( $name, $exp)= each %$index)
          { $self->setTwigHandler( $exp, sub { push @{$_[0]->{_twig_index}->{$name}}, $_; 1; }); }
      }

    $self->{twig_elt_class}= $args{EltClass} || 'XML::Twig::Elt';
    if( defined( $args{EltClass}) && $args{EltClass} ne 'XML::Twig::Elt') { $self->{twig_alt_elt_class}=1; }
    if( exists( $args{EltClass})) { delete $args{EltClass}; }

    if( exists( $args{MapXmlns}))
      { $self->{twig_map_xmlns}=  $args{MapXmlns};
        $self->{Namespaces}=1;
        delete $args{MapXmlns};
      }

    if( exists( $args{KeepOriginalPrefix}))
      { $self->{twig_keep_original_prefix}= $args{KeepOriginalPrefix};
        delete $args{KeepOriginalPrefix};
      }

    $self->{twig_dtd_handler}= $args{DTDHandler};
    delete $args{DTDHandler};

    if( $args{ExpandExternalEnts})
      { $self->set_expand_external_entities( 1);
        $self->{twig_expand_external_ents}= $args{ExpandExternalEnts}; 
        $self->{twig_read_external_dtd}= 1; # implied by ExpandExternalEnts
        if( $args{ExpandExternalEnts} == -1) 
          { $self->{twig_extern_ent_nofail}= 1;
            $self->setHandlers( ExternEnt => \&_twig_extern_ent_nofail);
          }
        delete $args{LoadDTD};
        delete $args{ExpandExternalEnts};
      }
    else
      { $self->set_expand_external_entities( 0); }

    if( !$args{NoLWP} && ! _use( 'URI') && ! _use( 'URI::File') && ! _use( 'LWP'))
      { $self->{twig_ext_ent_handler}= \&XML::Parser::initial_ext_ent_handler }
    else
      { $self->{twig_ext_ent_handler}= \&XML::Parser::file_ext_ent_handler }

    if( $args{DoNotEscapeAmpInAtts})
      { $self->set_do_not_escape_amp_in_atts( 1); 
        $self->{twig_do_not_escape_amp_in_atts}=1;
      }
    else
      { $self->set_do_not_escape_amp_in_atts( 0); 
        $self->{twig_do_not_escape_amp_in_atts}=0;
      }

    # deal with TwigRoots argument, a hash of elements for which
    # subtrees will be built (and associated handlers)
     
    if( $args{TwigRoots})
      { $self->setTwigRoots( $args{TwigRoots});
        delete $args{TwigRoots}; 
      }
    
    if( $args{EndTagHandlers})
      { unless ($self->{twig_roots} || $args{ForceEndTagHandlersUsage})
          { croak "you should not use EndTagHandlers without TwigRoots\n",
                  "if you want to use it anyway, normally because you have ",
                  "a start_tag_handlers that calls 'ignore' and you want to ",
                  "call an ent_tag_handlers at the end of the element, then ",
                  "pass 'force_end_tag_handlers_usage => 1' as an argument ",
                  "to new";
          }
                  
        $self->setEndTagHandlers( $args{EndTagHandlers});
        delete $args{EndTagHandlers};
      }
      
    if( $args{TwigPrintOutsideRoots})
      { croak "cannot use twig_print_outside_roots without twig_roots"
          unless( $self->{twig_roots});
        # if the arg is a filehandle then store it
        if( _is_fh( $args{TwigPrintOutsideRoots}) )
          { $self->{twig_output_fh}= $args{TwigPrintOutsideRoots}; }
        $self->{twig_default_print}= $args{TwigPrintOutsideRoots};
      }

    # space policy
    if( $args{KeepSpaces})
      { croak "cannot use both keep_spaces and discard_spaces"        if( $args{DiscardSpaces});
        croak "cannot use both keep_spaces and discard_all_spaces"    if( $args{DiscardAllSpaces});
        croak "cannot use both keep_spaces and keep_spaces_in"        if( $args{KeepSpacesIn});
        $self->{twig_keep_spaces}=1;
        delete $args{KeepSpaces}; 
      }
    if( $args{DiscardSpaces})
      { 
        croak "cannot use both discard_spaces and keep_spaces_in"     if( $args{KeepSpacesIn});
        croak "cannot use both discard_spaces and discard_all_spaces" if( $args{DiscardAllSpaces});
        croak "cannot use both discard_spaces and discard_spaces_in"  if( $args{DiscardSpacesIn});
        $self->{twig_discard_spaces}=1; 
        delete $args{DiscardSpaces}; 
      }
    if( $args{KeepSpacesIn})
      { croak "cannot use both keep_spaces_in and discard_spaces_in"  if( $args{DiscardSpacesIn});
        croak "cannot use both keep_spaces_in and discard_all_spaces" if( $args{DiscardAllSpaces});
        $self->{twig_discard_spaces}=1; 
        $self->{twig_keep_spaces_in}={}; 
        my @tags= @{$args{KeepSpacesIn}}; 
        foreach my $tag (@tags) { $self->{twig_keep_spaces_in}->{$tag}=1; } 
        delete $args{KeepSpacesIn}; 
      }

    if( $args{DiscardAllSpaces})
      { 
        croak "cannot use both discard_all_spaces and discard_spaces_in" if( $args{DiscardSpacesIn});
        $self->{twig_discard_all_spaces}=1; 
        delete $args{DiscardAllSpaces}; 
      }

    if( $args{DiscardSpacesIn})
      { $self->{twig_keep_spaces}=1; 
        $self->{twig_discard_spaces_in}={}; 
        my @tags= @{$args{DiscardSpacesIn}};
        foreach my $tag (@tags) { $self->{twig_discard_spaces_in}->{$tag}=1; } 
        delete $args{DiscardSpacesIn}; 
      }
    # discard spaces by default 
    $self->{twig_discard_spaces}= 1 unless(  $self->{twig_keep_spaces});

    $args{Comments}||= $COMMENTS_DEFAULT;
    if( $args{Comments} eq 'drop')       { $self->{twig_keep_comments}= 0;    }
    elsif( $args{Comments} eq 'keep')    { $self->{twig_keep_comments}= 1;    }
    elsif( $args{Comments} eq 'process') { $self->{twig_process_comments}= 1; }
    else { croak "wrong value for comments argument: '$args{Comments}' (should be 'drop', 'keep' or 'process')"; }
    delete $args{Comments};

    $args{Pi}||= $PI_DEFAULT;
    if( $args{Pi} eq 'drop')       { $self->{twig_keep_pi}= 0;    }
    elsif( $args{Pi} eq 'keep')    { $self->{twig_keep_pi}= 1;    }
    elsif( $args{Pi} eq 'process') { $self->{twig_process_pi}= 1; }
    else { croak "wrong value for pi argument: '$args{Pi}' (should be 'drop', 'keep' or 'process')"; }
    delete $args{Pi};

    if( $args{KeepEncoding})
      { 
        # set it in XML::Twig::Elt so print functions know what to do
        $self->set_keep_encoding( 1); 
        $self->{parse_start_tag}= $args{ParseStartTag} || \&_parse_start_tag; 
        delete $args{ParseStartTag} if defined( $args{ParseStartTag}) ;
        delete $args{KeepEncoding};
      }
    else
      { $self->set_keep_encoding( 0);  
        if( $args{ParseStartTag}) 
          { $self->{parse_start_tag}= $args{ParseStartTag}; }
        else
          { delete $self->{parse_start_tag}; }
        delete $args{ParseStartTag};
      }

    if( $args{OutputFilter})
      { $self->set_output_filter( $args{OutputFilter}); 
        delete $args{OutputFilter};
      }
    else
      { $self->set_output_filter( 0); }

    if( $args{RemoveCdata})
      { $self->set_remove_cdata( $args{RemoveCdata}); 
        delete $args{RemoveCdata}; 
      }
    else
      { $self->set_remove_cdata( 0); }

    if( $args{OutputTextFilter})
      { $self->set_output_text_filter( $args{OutputTextFilter}); 
        delete $args{OutputTextFilter};
      }
    else
      { $self->set_output_text_filter( 0); }

    if( exists $args{KeepAttsOrder})
      { $self->{keep_atts_order}= $args{KeepAttsOrder};
        if( _use( 'Tie::IxHash'))
          { $self->set_keep_atts_order(  $self->{keep_atts_order}); }
        else 
          { croak "Tie::IxHash not available, option keep_atts_order not allowed"; }
      }
    else
      { $self->set_keep_atts_order( 0); }


    if( $args{PrettyPrint})    { $self->set_pretty_print( $args{PrettyPrint}); }
    if( $args{EscapeGt})       { $self->escape_gt( $args{EscapeGt});           }
    if( $args{EmptyTags})      { $self->set_empty_tag_style( $args{EmptyTags}) }

    if( exists $args{Id})      { $ID= $args{Id};                     delete $args{ID};             }
    if( $args{NoProlog})       { $self->{no_prolog}= 1;              delete $args{NoProlog};       }
    if( $args{DoNotOutputDTD}) { $self->{no_dtd_output}= 1;          delete $args{DoNotOutputDTD}; }
    if( $args{LoadDTD})        { $self->{twig_read_external_dtd}= 1; delete $args{LoadDTD};        }
    if( $args{CharHandler})    { $self->setCharHandler( $args{CharHandler}); delete $args{CharHandler}; }

    if( $args{InputFilter})    { $self->set_input_filter(  $args{InputFilter}); delete  $args{InputFilter}; }
    if( $args{NoExpand})       { $self->setHandlers( %twig_noexpand_handlers); $self->{twig_no_expand}=1; }
    if( my $output_encoding= $args{OutputEncoding}) { $self->set_output_encoding( $output_encoding); delete $args{OutputFilter}; }

    if( my $tdh= $args{TopDownHandlers}) { $self->{twig_tdh}=1; delete $args{TopDownHandlers}; }

    if( my $acc_a= $args{AttAccessors})   { $self->att_accessors( @$acc_a);  }
    if( my $acc_e= $args{EltAccessors})   { $self->elt_accessors( isa( $acc_e, 'ARRAY') ? @$acc_e : $acc_e);   }
    if( my $acc_f= $args{FieldAccessors}) { $self->field_accessors( @$acc_f); }

    if( $args{UseTidy}) { $self->{use_tidy}= 1; }
    $self->{tidy_options}= $args{TidyOptions} || {};

    if( $args{OutputHtmlDoctype}) { $self->{html_doctype}= 1; }

    $self->set_quote( $args{Quote} || 'double');

    # set handlers
    if( $self->{twig_roots})
      { if( $self->{twig_default_print})
          { if( $self->{twig_keep_encoding})
              { $self->setHandlers( %twig_handlers_roots_print_original); }
            else
              { $self->setHandlers( %twig_handlers_roots_print);  }
          }
        else
          { $self->setHandlers( %twig_handlers_roots); }
      }
    else
      { $self->setHandlers( %twig_handlers); }

    # XML::Parser::Expat does not like these handler to be set. So in order to 
    # use the various sets of handlers on XML::Parser or XML::Parser::Expat
    # objects when needed, these ones have to be set only once, here, at 
    # XML::Parser level
    $self->setHandlers( Init => \&_twig_init, Final => \&_twig_final);

    $self->{twig_entity_list}= XML::Twig::Entity_list->new; 

    $self->{twig_id}= $ID; 
    $self->{twig_stored_spaces}='';

    $self->{twig_autoflush}= 1; # auto flush by default

    $self->{twig}= $self;
    if( $weakrefs) { weaken( $self->{twig}); }

    return $self;
  }

sub parse
  {
    my $t= shift;
    # if called as a class method, calls nparse, which creates the twig then parses it
    if( !ref( $t) || !isa( $t, 'XML::Twig')) { return $t->nparse( @_); }

    # requires 5.006 at least (or the ${^UNICODE} causes a problem)                                       # > perl 5.5
    # trap underlying bug in IO::Handle (see RT #17500)                                                   # > perl 5.5
    # croak if perl 5.8+, -CD (or PERL_UNICODE set to D) and parsing a pipe                               # > perl 5.5
    if( $perl_version>=5.008 && ${^UNICODE} && (${^UNICODE} & 24) && isa( $_[0], 'GLOB') && -p $_[0] )               # > perl 5.5
      { croak   "cannot parse the output of a pipe when perl is set to use the UTF8 perlIO layer\n"       # > perl 5.5
              . "set the environment variable PERL_UNICODE or use the -C option (see perldoc perlrun)\n"  # > perl 5.5
              . "not to include 'D'";                                                                     # > perl 5.5
      }                                                                                                   # > perl 5.5
    $t= eval { $t->SUPER::parse( @_); }; 
    
    if(    !$t 
        && $@=~m{(syntax error at line 1, column 0, byte 0|not well-formed \(invalid token\) at line 1, column 1, byte 1)} 
        && -f $_[0]
      )
      { croak "you seem to have used the parse method on a filename ($_[0]), you probably want parsefile instead"; }
    return _checked_parse_result( $t, $@);
  }

sub parsefile
  { my $t= shift;
    if( -f $_[0] && ! -s $_[0]) { return _checked_parse_result( undef, "empty file '$_[0]'"); }
    $t= eval { $t->SUPER::parsefile( @_); };
    return _checked_parse_result( $t, $@);
  }

sub _checked_parse_result
  { my( $t, $returned)= @_;
    if( !$t)
      { if( isa( $returned, 'XML::Twig') && $returned->{twig_finish_now})
          { $t= $returned;
            delete $t->{twig_finish_now};
            return $t->_twig_final;
          }
        else
          { _croak( $returned, 0); }
      }
    
    $active_twig= $t;
    return $t;
  }

sub active_twig { return $active_twig; }

sub finish_now
  { my $t= shift;
    $t->{twig_finish_now}=1;
    die $t;    
  }


sub parsefile_inplace      { shift->_parse_inplace( parsefile      => @_); }
sub parsefile_html_inplace { shift->_parse_inplace( parsefile_html => @_); }

sub _parse_inplace
  { my( $t, $method, $file, $suffix)= @_;
    _use( 'File::Temp') || croak "need File::Temp to use inplace methods\n";
    _use( 'File::Basename');


    my $tmpdir= dirname( $file);
    my( $tmpfh, $tmpfile)= File::Temp::tempfile( DIR => $tmpdir);
    my $original_fh= select $tmpfh;

    unless( $t->{twig_keep_encoding} || $perl_version < 5.006) 
      { if( grep /useperlio=define/, `$^X -V`) # we can only use binmode :utf8 if perl was compiled with useperlio
          { binmode( $tmpfh, ":utf8" ); }
      }

    $t->$method( $file);

    select $original_fh;
    close $tmpfh;
    my $mode= (stat( $file))[2] & oct(7777);
    chmod $mode, $tmpfile or croak "cannot change temp file mode to $mode: $!";

    if( $suffix) 
      { my $backup;
        if( $suffix=~ m{\*}) { ($backup = $suffix) =~ s/\*/$file/g; }
        else                 { $backup= $file . $suffix; }
          
        rename( $file, $backup) or croak "cannot backup initial file ($file) to $backup: $!"; 
      }
    rename( $tmpfile, $file) or croak "cannot rename temp file ($tmpfile) to initial file ($file): $!";

    return $t;
  }
    
 
sub parseurl
  { my $t= shift;
    $t->_parseurl( 0, @_);
  }

sub safe_parseurl
  { my $t= shift;
    $t->_parseurl( 1, @_);
  }

sub safe_parsefile_html
  { my $t= shift;
    eval { $t->parsefile_html( @_); };
    return $@ ? $t->_reset_twig_after_error : $t;
  }

sub safe_parseurl_html
  { my $t= shift;
    _use( 'LWP::Simple') or croak "missing LWP::Simple"; 
    eval { $t->parse_html( LWP::Simple::get( shift()), @_); } ;
    return $@ ? $t->_reset_twig_after_error : $t;
  }

sub parseurl_html
  { my $t= shift;
    _use( 'LWP::Simple') or croak "missing LWP::Simple"; 
    $t->parse_html( LWP::Simple::get( shift()), @_); 
  }


# uses eval to catch the parser's death
sub safe_parse_html
  { my $t= shift;
    eval { $t->parse_html( @_); } ;
    return $@ ? $t->_reset_twig_after_error : $t;
  }

sub parsefile_html
  { my $t= shift;
    my $file= shift;
    my $indent= $t->{ErrorContext} ? 1 : 0;
    $t->set_empty_tag_style( 'html');
    my $html2xml=  $t->{use_tidy} ? \&_tidy_html : \&_html2xml;
    my $options= $t->{use_tidy} ? $t->{tidy_options} || {} :  { indent => $indent, html_doctype => $t->{html_doctype} };
    $t->parse( $html2xml->( _slurp( $file), $options), @_);
    return $t;
  }

sub parse_html
  { my $t= shift;
    my $content= shift;
    my $indent= $t->{ErrorContext} ? 1 : 0;
    $t->set_empty_tag_style( 'html');
    my $html2xml=  $t->{use_tidy} ? \&_tidy_html : \&_html2xml;
    my $options= $t->{use_tidy} ? $t->{tidy_options} || {} :  { indent => $indent, html_doctype => $t->{html_doctype} };
    $t->parse( $html2xml->( isa( $content, 'GLOB') ? _slurp_fh( $content) : $content, $options), @_);
    return $t;
  }

sub xparse
  { my $t= shift;
    my $to_parse= $_[0];
    if( isa( $to_parse, 'GLOB'))             { $t->parse( @_);                 }
    elsif( $to_parse=~ m{^\s*<})             { $to_parse=~ m{<html}i ? $t->_parse_as_xml_or_html( @_)
                                                                     : $t->parse( @_);                 
                                             }
    elsif( $to_parse=~ m{^\w+://.*\.html?$}) { _use( 'LWP::Simple') or croak "missing LWP::Simple"; 
                                               $t->_parse_as_xml_or_html( LWP::Simple::get( shift()), @_);
                                             }
    elsif( $to_parse=~ m{^\w+://})           { _use( 'LWP::Simple') or croak "missing LWP::Simple";
                                               my $doc= LWP::Simple::get( shift);
                                               if( ! defined $doc) { $doc=''; }
                                               my $xml_parse_ok= $t->safe_parse( $doc, @_);
                                               if( $xml_parse_ok)
                                                 { return $xml_parse_ok; }
                                               else
                                                 { my $diag= $@;
                                                   if( $doc=~ m{<html}i)
                                                     { $t->parse_html( $doc, @_); }
                                                    else
                                                      { croak $diag; }
                                                 }
                                             }
    elsif( $to_parse=~ m{\.html?$})          { my $content= _slurp( shift);
                                               $t->_parse_as_xml_or_html( $content, @_); 
                                             }
    else                                     { $t->parsefile( @_);             }
  }

sub _parse_as_xml_or_html
  { my $t= shift; 
    if( _is_well_formed_xml( $_[0]))
      { $t->parse( @_) }
    else
      { my $html2xml=  $t->{use_tidy} ? \&_tidy_html : \&_html2xml;
        my $options= $t->{use_tidy} ? $t->{tidy_options} || {} :  { indent => 0, html_doctype => $t->{html_doctype} };
        my $html= $html2xml->( $_[0], $options, @_);
        if( _is_well_formed_xml( $html))
          { $t->parse( $html); }
        else
          { croak $@; } # can't really test this because HTML::Parser or HTML::Tidy may change how they deal with bas HTML between versions
      }
  }  
    
{ my $parser;
  sub _is_well_formed_xml
    { $parser ||= XML::Parser->new;
      eval { $parser->parse( $_[0]); };
      return $@ ? 0 : 1;
    }
}

sub nparse
  { my $class= shift;
    my $to_parse= pop;
    $class->new( @_)->xparse( $to_parse);
  }

sub nparse_pp   { shift()->nparse( pretty_print => 'indented', @_); }
sub nparse_e    { shift()->nparse( error_context => 1,         @_); }
sub nparse_ppe  { shift()->nparse( pretty_print => 'indented', error_context => 1, @_); }


sub _html2xml
  { my( $html, $options)= @_;
    _use( 'HTML::TreeBuilder', '3.13') or croak "cannot parse HTML: missing HTML::TreeBuilder v >= 3.13\n"; 
    my $tree= HTML::TreeBuilder->new;
    $tree->ignore_ignorable_whitespace( 0); 
    $tree->ignore_unknown( 0); 
    $tree->no_space_compacting( 1);
    $tree->store_comments( 1);
    $tree->store_pis(1); 
    $tree->parse( $html);
    $tree->eof;

    my $xml='';
    if( $options->{html_doctype} && exists $tree->{_decl} )
      { my $decl= $tree->{_decl}->as_XML;

        # first try to fix declarations that are missing the SYSTEM part 
        $decl =~ s{^\s*<!DOCTYPE \s+ ((?i)html) \s+ PUBLIC \s+ "([^"]*)" \s* >}
                  { my $system= $HTML_DECL{$2} || $HTML_DECL{$DEFAULT_HTML_TYPE};
                    qq{<!DOCTYPE $1 PUBLIC "$2" "$system">}
                   
                  }xe;

        # then check that the declaration looks OK (so it parses), if not remove it,
        # better to parse without the declaration than to die stupidely
        if(    $decl =~ m{<!DOCTYPE \s+ (?i:HTML) (\s+ PUBLIC \s+ "[^"]*" \s+ (SYSTEM \s+)? "[^"]*")? \s*>}x # PUBLIC then SYSTEM
            || $decl =~ m{<!DOCTYPE \s+ (?i:HTML) \s+ SYSTEM \s+ "[^"]*" \s*>}x                             # just SYSTEM
          )
          { $xml= $decl; }
      } 

    $xml.= _as_XML( $tree);

    _fix_xml( $tree, \$xml);

    if( $options->{indent}) { _indent_xhtml( \$xml); }
    $tree->delete;
    $xml=~ s{\s+$}{}s; # trim end
    return $xml;
  }

sub _tidy_html
  { my( $html, $options)= @_;
   _use( 'HTML::Tidy') or croak "cannot cleanup HTML using HTML::Tidy (required by the use_tidy option): $@\n"; ;
    my $TIDY_DEFAULTS= { output_xhtml => 1, # duh!
                         tidy_mark => 0,    # do not add the "generated by tidy" comment
                         numeric_entities => 1,
                         char_encoding =>  'utf8',
                         bare => 1,
                         clean => 1,
                         doctype => 'transitional',
                         fix_backslash => 1,
                         merge_divs => 0,
                         merge_spans => 0,
                         sort_attributes => 'alpha',
                         indent => 0,
                         wrap => 0,
                         break_before_br => 0,
                       };
    $options ||= {};
    my $tidy_options= { %$TIDY_DEFAULTS, %$options};
    my $tidy = HTML::Tidy->new( $tidy_options);
    $tidy->ignore( type => 1, type => 2 ); # 1 is TIDY_WARNING, 2 is TIDY_ERROR, not clean
    my $xml= $tidy->clean( $html );
    return $xml;
  }


{ my %xml_parser_encoding;
  sub _fix_xml
    { my( $tree, $xml)= @_; # $xml is a ref to the xml string

      my $max_tries=5;
      my $add_decl;

      while( ! _check_xml( $xml) && $max_tries--)
        { 
          # a couple of fixes for weird HTML::TreeBuilder errors
          if( $@=~ m{^\s*xml (or text )?declaration not at start of (external )?entity}i)
            { $$xml=~ s{<\?xml.*?\?>}{}g; 
              #warn " fixed xml declaration in the wrong place\n";
            }
          elsif( $@=~ m{undefined entity})
            { $$xml=~ s{&(amp;)?Amp;}{&amp;}g if $HTML::TreeBuilder::VERSION < 4.00;
              if( _use( 'HTML::Entities::Numbered')) { $$xml=name2hex_xml( $$xml); }
              $$xml=~ s{&(\w+);}{ my $ent= $1; if( $ent !~ m{^(amp|lt|gt|apos|quote)$}) { "&amp;$ent;" } }eg;
            }
          elsif( $@=~ m{&Amp; used in html})
            # if $Amp; is used instead of &amp; then HTML::TreeBuilder's as_xml is tripped (old version)
            { $$xml=~ s{&(amp;)?Amp;}{&amp;}g if $HTML::TreeBuilder::VERSION < 4.00; 
            } 
          elsif( $@=~ m{^\s*not well-formed \(invalid token\)})
            { if( $HTML::TreeBuilder::VERSION < 4.00)
                { $$xml=~ s{&(amp;)?Amp;}{&amp;}g; 
                  $$xml=~  s{(<[^>]* )(\d+=)"}{$1a$2"}g; # <table 1> comes out as <table 1="1">, "fix the attribute
                }
              my $q= '<img "="&#34;" '; # extracted so vim doesn't get confused
              if( _use( 'HTML::Entities::Numbered')) { $$xml=name2hex_xml( $$xml); }
              if( $$xml=~ m{$q}) 
                { $$xml=~ s{$q}{<img }g; # happens with <img src="foo.png"" ...
                } 
              else
                { my $encoding= _encoding_from_meta( $tree);
                  unless( keys %xml_parser_encoding) { %xml_parser_encoding= _xml_parser_encodings(); }

                  if( ! $add_decl)
                    { if( $xml_parser_encoding{$encoding})
                        { $add_decl=1; }
                      elsif( $encoding eq 'euc-jp' && $xml_parser_encoding{'x-euc-jp-jisx0221'})
                        { $encoding="x-euc-jp-jisx0221"; $add_decl=1;}
                      elsif( $encoding eq 'shift-jis' && $xml_parser_encoding{'x-sjis-jisx0221'})
                        { $encoding="x-sjis-jisx0221";   $add_decl=1;}

                      if( $add_decl) 
                        { $$xml=~ s{^(<\?xml.*?\?>)?}{<?xml version="1.0" encoding="$encoding"?>}s;
                          #warn "  added decl (encoding $encoding)\n";
                        }
                      else
                        { $$xml=~ s{^(<\?xml.*?\?>)?}{}s;
                          #warn "  converting to utf8 from $encoding\n";
                          $$xml= _to_utf8( $encoding, $$xml);
                        }
                    }
                  else
                    { $$xml=~ s{^(<\?xml.*?\?>)?}{}s;
                      #warn "  converting to utf8 from $encoding\n";
                      $$xml= _to_utf8( $encoding, $$xml);
                    }
                }
            }
        }

      # some versions of HTML::TreeBuilder escape CDATA sections
      $$xml=~ s{(&lt;!\[CDATA\[.*?\]\]&gt;)}{_unescape_cdata( $1)}eg;
    
  }

  sub _xml_parser_encodings
    { my @encodings=( 'iso-8859-1'); # this one is included by default, there is no map for it in @INC
      foreach my $inc (@INC)
        { push @encodings, map { basename( $_, '.enc') } glob( File::Spec->catdir( $inc => XML => Parser => Encodings => '*.enc')); }
      return map { $_ => 1 } @encodings;
    }
}


sub _unescape_cdata
  { my( $cdata)= @_;
    $cdata=~s{&lt;}{<}g;
    $cdata=~s{&gt;}{>}g;
    $cdata=~s{&amp;}{&}g;
    return $cdata;
  }

sub _as_XML {

    # fork of HTML::Element::as_XML, which is a little too buggy and inconsistent between versions for my liking
    my ($elt) = @_;
    my $xml= '';
    my $empty_element_map = $elt->_empty_element_map;

    my ( $tag, $node, $start );    # per-iteration scratch
    $elt->traverse(
        sub {
            ( $node, $start ) = @_;
            if ( ref $node ) 
              { # it's an element
                $tag = $node->{'_tag'};
                if ($start)
                  { # on the way in
                    foreach my $att ( grep { ! m{^(_|/$)} } keys %$node ) 
                       { # fix attribute names instead of dying
                         my $new_att= $att;
                         if( $att=~ m{^\d}) { $new_att= "a$att"; }
                         $new_att=~ s{[^\w\d:_-]}{}g;
                         $new_att ||= 'a'; 
                         if( $new_att ne $att) { $node->{$new_att}= delete $node->{$att}; }
                       }

                    if ( $empty_element_map->{$tag} and !@{ $node->{'_content'} || []} )
                      { $xml.= $node->starttag_XML( undef, 1 ); }
                    else 
                      { $xml.= $node->starttag_XML(undef); }
                  }
                else
                 { # on the way out
                   unless ( $empty_element_map->{$tag} and !@{ $node->{'_content'} || [] } )
                    { $xml.= $node->endtag_XML();
                    }     # otherwise it will have been an <... /> tag.
                  }
              }
            elsif( $node=~ /<!\[CDATA\[/)  # the content includes CDATA
              { 
                foreach my $chunk (split /(<!\[CDATA\[.*?\]\]>)/, $node) # chunks are CDATA sections or normal text
                  { $xml.= $chunk !~ m{<!\[CDATA\[} ? $chunk : _xml_escape( $chunk); }
              }
            else   # it's just text
              { $xml .= _xml_escape($node); }
            1;            # keep traversing
        }
    );
  return $xml;
}

sub _xml_escape 
  { my( $html)= @_;
    $html =~ s{&(?!                     # An ampersand that isn't followed by...
                  (  \#[0-9]+;       |  #   A hash mark, digits and semicolon, or
                    \#x[0-9a-fA-F]+; |  #   A hash mark, "x", hex digits and semicolon, or
                    [\w]+               #   A valid unicode entity name and semicolon
                  )
                )
              }
              {&amp;}gx;    # Needs to be escaped to amp

    # in old versions of HTML::TreeBuilder &amp; can come out as &Amp;
    if( $HTML::TreeBuilder::VERSION <= 3.23) { $html=~ s{&Amp;}{&amp;}g; }

    # simple character escapes
    $html =~ s/</&lt;/g;
    $html =~ s/>/&gt;/g;
    $html =~ s/"/&quot;/g;
    $html =~ s/'/&apos;/g;

    return $html;
  }




sub _check_xml
  { my( $xml)= @_; # $xml is a ref to the xml string
    my $ok= eval { XML::Parser->new->parse( $$xml); };
    #if( $ok) { warn "  parse OK\n"; }
    return $ok;
  }

sub _encoding_from_meta
  { my( $tree)= @_; 
    my $enc="iso-8859-1";
    my @meta= $tree->find( 'meta');
    foreach my $meta (@meta)
      { if(    $meta->{'http-equiv'} && ($meta->{'http-equiv'} =~ m{^\s*content-type\s*}i)
            && $meta->{content}      && ($meta->{content}      =~ m{^\s*text/html\s*;\s*charset\s*=\s*(\S*)\s*}i)
          )
          { $enc= lc $1;
            #warn "  encoding from meta tag is '$enc'\n";
            last;
          }
      }
    return $enc;
  }

{ sub _to_utf8 
    { my( $encoding, $string)= @_;
      local $SIG{__DIE__};
      if( _use(  'Encode')) 
        { Encode::from_to( $string, $encoding => 'utf8', 0x0400); } # 0x0400 is Encode::FB_XMLCREF
      elsif( _use( 'Text::Iconv'))
        { my $converter =  eval { Text::Iconv->new( $encoding => "utf8") };
          if( $converter) {  $string= $converter->convert( $string); }
        }
      elsif( _use( 'Unicode::Map8') && _use( 'Unicode::String'))
        { my $map= Unicode::Map8->new( $encoding); 
          $string= $map->tou( $string)->utf8;
        }
      $string=~ s{[\x00-\x08\x0B\x0C\x0E-\x1F]}{}g; # get rid of control chars, portable in 5.6
    return $string;
  }
}


sub _indent_xhtml
  { my( $xhtml)= @_; # $xhtml is a ref
    my %block_tag= map { $_ => 1 } qw( html 
                                         head 
                                           meta title link script base
                                         body 
                                           h1 h2 h3 h4 h5 h6 
                                           p br address  blockquote pre 
                                           ol ul li  dd dl dt 
                                           table tr td th tbody tfoot thead  col colgroup caption 
                                           div frame frameset hr
                                     ); 

    my $level=0;
    $$xhtml=~ s{( (?:<!(?:--.*?-->|[CDATA[.*?]]>)) # ignore comments and CDATA sections
                  | <(\w+)((?:\s+\w+\s*=\s*(?:"[^"]*"|'[^']*'))*\s*/>) # empty tag
                  | <(\w+)                         # start tag
                  |</(\w+)                         # end tag 
                )
               }
               { if(    $2 && $block_tag{$2})  { my $indent= "  " x $level;
                                                 "\n$indent<$2$3"; 
                                               }
                 elsif( $4 && $block_tag{$4})  { my $indent= "  " x $level; 
                                                 $level++ unless( $4=~ m{/>});
                                                 my $nl= $4 eq 'html' ? '' : "\n";
                                                 "$nl$indent<$4"; 
                                               }
                 elsif( $5  && $block_tag{$5}) { $level--; "</$5"; }
                 else                          { $1; }
               }xesg;
  }


sub add_stylesheet
  { my( $t, $type, $href)= @_;
    my %text_type= map { $_ => 1 } qw( xsl css);
    my $ss= $t->{twig_elt_class}->new( $PI);
    if( $text_type{$type}) 
      { $ss->_set_pi( 'xml-stylesheet', qq{type="text/$type" href="$href"}); }
    else
      { croak "unsupported style sheet type '$type'"; }
      
    $t->_add_cpi_outside_of_root( leading_cpi => $ss);
    return $t;
  }

{ my %used;       # module => 1 if require ok, 0 otherwise
  my %disallowed; # for testing, refuses to _use modules in this hash

  sub _disallow_use ## no critic (Subroutines::ProhibitNestedSubs);
    { my( @modules)= @_;
      $disallowed{$_}= 1 foreach (@modules);
    }

  sub _allow_use  ## no critic (Subroutines::ProhibitNestedSubs);
    { my( @modules)= @_;
      $disallowed{$_}= 0 foreach (@modules);
    }

  sub _use  ## no critic (Subroutines::ProhibitNestedSubs);
    { my( $module, $version)= @_;
      $version ||= 0;
      if( $disallowed{$module})   { return 0; }
      if( $used{$module})         { return 1; }
      if( eval "require $module") { import $module; $used{$module}= 1; 
                                    if( $version)
                                      { 
                                        ## no critic (TestingAndDebugging::ProhibitNoStrict);
                                        no strict 'refs';
                                        if( ${"${module}::VERSION"} >= $version ) { return 1; }
                                        else                                      { return 0; }
                                      }
                                    else
                                      { return 1; }
                                  }
      else                        {                          $used{$module}= 0; return 0; }
    }
}

# used to solve the [n] predicates while avoiding getting the entire list
# needs a prototype to accept passing bare blocks
sub _first_n(&$@)       ## nocritic (Subroutines::ProhibitSubroutinePrototypes);
  { my $coderef= shift;
    my $n= shift;         
    my $i=0;
    if( $n > 0)
      { foreach (@_)         { if( &$coderef) { $i++; return $_ if( $i == $n); } } }
    elsif( $n < 0)
      { foreach (reverse @_) { if( &$coderef) { $i--; return $_ if( $i == $n); } } }
    else
      { croak "illegal position number 0"; }
    return undef;
  }

sub _slurp_uri
  { my( $uri, $base)= @_;
    if( $uri=~ m{^\w+://}) { _use( 'LWP::Simple'); return LWP::Simple::get( $uri); }
    else                   { return _slurp( _based_filename( $uri, $base));        }
  }

sub _based_filename
  { my( $filename, $base)= @_;
    # cf. XML/Parser.pm's file_ext_ent_handler
    if (defined($base) and not ($filename =~ m{^(?:[\\/]|\w+:)})) 
          { my $newpath = $base;
            $newpath =~ s{[^\\/:]*$}{$filename};
            $filename = $newpath;
          }
    return $filename;
  }

sub _slurp
  { my( $filename)= @_;
    my $to_slurp;
    open( $to_slurp, "<$filename") or croak "cannot open '$filename': $!"; 
    local $/= undef;
    my $content= <$to_slurp>;
    close $to_slurp;
    return $content;
  }
  
sub _slurp_fh
  { my( $fh)= @_;
    local $/= undef;
    my $content= <$fh>;
    return $content;
  }    
 
# I should really add extra options to allow better configuration of the 
# LWP::UserAgent object
# this method forks (except on VMS!)
#   - the child gets the data and copies it to the pipe,
#   - the parent reads the stream and sends it to XML::Parser
# the data is cut it chunks the size of the XML::Parser::Expat buffer
# the method returns the twig and the status
sub _parseurl
  { my( $t, $safe, $url, $agent)= @_;
    _use( 'LWP') || croak "LWP not available, needed to use parseurl methods";
    if( $^O ne 'VMS')
      { pipe( README, WRITEME) or croak  "cannot create connected pipes: $!";
        if( my $pid= fork)
          { # parent code: parse the incoming file
            close WRITEME; # no need to write
            my $result= $safe ? $t->safe_parse( \*README) : $t->parse( \*README);
            close README;
            return $@ ? 0 : $t;
          }
        else
         { # child
            close README; # no need to read
            local $|=1;
            $agent    ||= LWP::UserAgent->new;
            my $request  = HTTP::Request->new( GET => $url);
            # _pass_url_content is called with chunks of data the same size as
            # the XML::Parser buffer 
            my $response = $agent->request( $request, 
                             sub { _pass_url_content( \*WRITEME, @_); }, $BUFSIZE);
            $response->is_success or croak "$url ", $response->message;
            close WRITEME;
            CORE::exit(); # CORE is there for mod_perl (which redefines exit)
          }
      } 
    else 
      { # VMS branch (hard to test!)
        local $|=1;
        $agent    ||= LWP::UserAgent->new;
        my $request  = HTTP::Request->new( GET => $url);
        my $response = $agent->request( $request);
        $response->is_success or croak "$url ", $response->message;
        my $result= $safe ? $t->safe_parse($response->content) : $t->parse($response->content);
        return $@ ? 0 : $t;
     }

  }

# get the (hopefully!) XML data from the URL and 
sub _pass_url_content
  { my( $fh, $data, $response, $protocol)= @_;
    print {$fh} $data;
  }

sub add_options
  { my %args= map { $_, 1 } @_;
    %args= _normalize_args( %args);
    foreach (keys %args) { $valid_option{$_}++; } 
  }

sub _pretty_print_styles { return XML::Twig::Elt::_pretty_print_styles(); }

sub _twig_store_internal_dtd
 { 
   # warn " in _twig_store_internal_dtd...\n"; # DEBUG handler
    my( $p, $string)= @_;
    my $t= $p->{twig};
    if( $t->{twig_keep_encoding}) { $string= $p->original_string(); }
    $t->{twig_doctype}->{internal} .= $string;
    return;
  }

sub _twig_stop_storing_internal_dtd
   { # warn " in _twig_stop_storing_internal_dtd...\n"; # DEBUG handler
    my $p= shift;
    if( @saved_default_handler && defined $saved_default_handler[1])
      { $p->setHandlers( @saved_default_handler); }
    else
      { 
        $p->setHandlers( Default => undef);
      }
    $p->{twig}->{twig_doctype}->{internal}=~ s{^\s*\[}{};
    $p->{twig}->{twig_doctype}->{internal}=~ s{\]\s*$}{};
    return;
  }

sub _twig_doctype_fin_print
  { # warn " in _twig_doctype_fin_print...\n"; # DEBUG handler
    my( $p)= shift;
    if( $p->{twig}->{twig_doctype}->{has_internal} && !$expat_1_95_2) { print ' ]>'; }
    return;
  }
    

sub _normalize_args
  { my %normalized_args;
    while( my $key= shift )
      { $key= join '', map { ucfirst } split /_/, $key;
        #$key= "Twig".$key unless( substr( $key, 0, 4) eq 'Twig');
        $normalized_args{$key}= shift ;
      }
    return %normalized_args;
  }    

sub _is_fh { return unless $_[0]; return $_[0] if( isa( $_[0], 'GLOB') || isa( $_[0], 'IO::Scalar')); }

sub _set_handler
  { my( $handlers, $whole_path, $handler)= @_;

    my $H_SPECIAL = qr{($ALL|$DEFAULT|$COMMENT)};
    my $H_PI      = qr{(\?|$PI)\s*(([^\s]*)\s*)};
    my $H_LEVEL   = qr{level \s* \( \s* ([0-9]+) \s* \)}x;
    my $H_REGEXP  = qr{\(\?([\^xism]*)(-[\^xism]*)?:(.*)\)}x;
    my $H_XPATH   = qr{(/?/?$REG_NAME_WC? \s* ($REG_PREDICATE\s*)?)+}x;

    my $prev_handler;

    my $cpath= $whole_path;
    #warn "\$cpath: '$cpath\n";
    while( $cpath && $cpath=~ s{^\s*($H_SPECIAL|$H_PI|$H_LEVEL|$H_REGEXP|$H_XPATH)\s*($|\|)}{})
      { my $path= $1;
        #warn "\$cpath: '$cpath' - $path: '$path'\n";
        $prev_handler ||= $handlers->{handlers}->{string}->{$path} || undef; # $prev_handler gets the first found handler

           _set_special_handler         ( $handlers, $path, $handler, $prev_handler)
        || _set_pi_handler              ( $handlers, $path, $handler, $prev_handler)
        || _set_level_handler           ( $handlers, $path, $handler, $prev_handler)
        || _set_regexp_handler          ( $handlers, $path, $handler, $prev_handler)
        || _set_xpath_handler           ( $handlers, $path, $handler, $prev_handler)
        || croak "unrecognized expression in handler: '$whole_path'";
    
        $handlers->{handlers}->{string}->{$path}= $handler;
      }

    if( $cpath) { croak "unrecognized expression in handler: '$whole_path'"; }

    # this both takes care of the simple (gi) handlers and store
    # the handler code reference for other handlers

    return $prev_handler;
  }


sub _set_special_handler
  { my( $handlers, $path, $handler, $prev_handler)= @_;
    if( $path =~ m{^\s*($ALL|$DEFAULT|$COMMENT)\s*$}io )
      { $handlers->{handlers}->{$1}= $handler; 
        return 1;
      }
    else 
      { return 0; }
  }

sub _set_xpath_handler
  { my( $handlers, $path, $handler, $prev_handler)= @_;
    if( my $handler_data= _parse_xpath_handler( $path, $handler))
      { _add_handler( $handlers, $handler_data, $path, $prev_handler);
        return 1;
      }
    else 
      { return 0; }
  }

sub _add_handler
  { my( $handlers, $handler_data, $path, $prev_handler)= @_;

    my $tag= $handler_data->{tag};
    my @handlers= $handlers->{xpath_handler}->{$tag} ? @{$handlers->{xpath_handler}->{$tag}} : ();

    if( $prev_handler) { @handlers= grep { $_->{path} ne $path } @handlers; }

    push @handlers, $handler_data if( $handler_data->{handler});
    
    if( @handlers > 1)
      { @handlers= sort {    (($b->{score}->{type}        || 0)  <=>  ($a->{score}->{type}        || 0))
                          || (($b->{score}->{anchored}    || 0)  <=>  ($a->{score}->{anchored}    || 0))
                          || (($b->{score}->{steps}       || 0)  <=>  ($a->{score}->{steps}       || 0))
                          || (($b->{score}->{predicates}  || 0)  <=>  ($a->{score}->{predicates}  || 0))
                          || (($b->{score}->{tests}       || 0)  <=>  ($a->{score}->{tests}       || 0))
                          || ($a->{path} cmp $b->{path})
                        } @handlers;
      }

    $handlers->{xpath_handler}->{$tag}= \@handlers;
  }

sub _set_pi_handler
  { my( $handlers, $path, $handler, $prev_handler)= @_;
    # PI conditions ( '?target' => \&handler or '?' => \&handler
    #             or '#PItarget' => \&handler or '#PI' => \&handler)
    if( $path=~ /^\s*(?:\?|$PI)\s*(?:([^\s]*)\s*)$/)
      { my $target= $1 || '';
        # update the path_handlers count, knowing that
        # either the previous or the new handler can be undef
        $handlers->{pi_handlers}->{$1}= $handler;
        return 1;
      }
    else 
      { return 0; 
      }
  }

sub _set_level_handler
  { my( $handlers, $path, $handler, $prev_handler)= @_;
    if( $path =~ m{^ \s* level \s* \( \s* ([0-9]+) \s* \) \s* $}ox )
      { my $level= $1;
        my $sub= sub { my( $stack)= @_; return( ($stack->[-1]->{_tag} !~ m{^#}) && (scalar @$stack == $level + 1) ) }; 
        my $handler_data=  { tag=> '*', score => { type => $LEVEL_TRIGGER}, trigger => $sub, 
                             path => $path, handler => $handler, test_on_text => 0
                           };
        _add_handler( $handlers, $handler_data, $path, $prev_handler);
        return 1;
      }
    else 
      { return 0; }
  }

sub _set_regexp_handler
  { my( $handlers, $path, $handler, $prev_handler)= @_; 
    # if the expression was a regexp it is now a string (it was stringified when it became a hash key)
    if( $path=~ m{^\(\?([\^xism]*)(?:-[\^xism]*)?:(.*)\)$}) 
      { my $regexp= qr/(?$1:$2)/; # convert it back into a regexp
        my $sub= sub { my( $stack)= @_; return( $stack->[-1]->{_tag} =~ $regexp ) }; 
        my $handler_data=  { tag=> '*', score => { type => $REGEXP_TRIGGER} , trigger => $sub, 
                             path => $path, handler => $handler, test_on_text => 0 
                           };
        _add_handler( $handlers, $handler_data, $path, $prev_handler);
        return 1;
      }
    else 
      { return 0; }
  }

my $DEBUG_HANDLER= 0; # 0 or 1 (output the handler checking code) or 2 (super verbose)
my $handler_string;   # store the handler itself
sub _set_debug_handler    { $DEBUG_HANDLER= shift; }
sub _warn_debug_handler   { if( $DEBUG_HANDLER < 3) { warn @_; } else { $handler_string .= join( '', @_); } }
sub _return_debug_handler { my $string=  $handler_string; $handler_string=''; return $string; }

sub _parse_xpath_handler
  { my( $xpath, $handler)= @_;
    my $xpath_original= $xpath;


    if( $DEBUG_HANDLER >=1) { _warn_debug_handler( "\n\nparsing path '$xpath'\n"); }

    my $path_to_check= $xpath;
    $path_to_check=~ s{/?/?$REG_NAME_WC?\s*(?:$REG_PREDICATE\s*)?}{}g;
    if( $DEBUG_HANDLER && $path_to_check=~ /\S/) { _warn_debug_handler( "left: $path_to_check\n"); }
    return if( $path_to_check=~ /\S/);

    (my $xpath_to_display= $xpath)=~ s{(["{}'\[\]\@\$])}{\\$1}g;

    my @xpath_steps;
    my $last_token_is_sep;

    while( $xpath=~ s{^\s*
                       ( (//?)                                      # separator
                        | (?:$REG_NAME_WC\s*(?:$REG_PREDICATE\s*)?) # tag name and optional predicate
                        | (?:$REG_PREDICATE)                        # just a predicate
                       )
                     }
                     {}x
         )
      { # check that we have alternating separators and steps
        if( $2) # found a separator
          { if(  $last_token_is_sep) { return 0; } # 2 seps in a row
            $last_token_is_sep= 1;
          }
        else
          { if( defined( $last_token_is_sep) && !$last_token_is_sep) { return 0; } # 2 steps in a row
            $last_token_is_sep= 0;
          }

        push @xpath_steps, $1;
      }
    if( $last_token_is_sep) { return 0; } # expression cannot end with a separator 

    my $i=-1;

    my $perlfunc= _join_n( $NO_WARNINGS . ';',
                           q|my( $stack)= @_;                    |,
                           q|my @current_elts= (scalar @$stack); |,
                           q|my @new_current_elts;               |,
                           q|my $elt;                            |,
                           ($DEBUG_HANDLER >= 1) && (qq#warn q{checking path '$xpath_to_display'\n};#),
                         );


    my $last_tag='';
    my $anchored= $xpath_original=~ m{^\s*/(?!/)} ? 1 : 0; 
    my $score={ type => $XPATH_TRIGGER, anchored => $anchored };
    my $flag= { test_on_text => 0 };
    my $sep='/';  # '/' or '//'
    while( my $xpath_step= pop @xpath_steps)
      { my( $tag, $predicate)= $xpath_step =~ m{^($REG_NAME_WC)?(?:\[(.*)\])?\s*$};
        $score->{steps}++;
        $tag||='*';

        my $warn_empty_stack= $DEBUG_HANDLER >= 2 ? qq{warn "return with empty stack\\n";} : '';

        if( $predicate)
          { if( $DEBUG_HANDLER >= 2)  { _warn_debug_handler( "predicate is: '$predicate'\n"); }
            # changes $predicate (from an XPath expression to a Perl one)
            if( $predicate=~ m{^\s*$REG_NUMBER\s*$}) { croak "position selector [$predicate] not supported on twig_handlers"; }
            _parse_predicate_in_handler( $predicate, $flag, $score);
            if( $DEBUG_HANDLER >= 2) { _warn_debug_handler( "predicate becomes: '$predicate'\n"); }
          }

       my $tag_cond=  _tag_cond( $tag);
       my $cond= join( " && ", grep { $_ } $tag_cond, $predicate) || 1;

       if( $css_sel && $tag=~ m{\.}) { $tag=~s{\.[^.]*$}{}; $tag ||='*'; }
       $last_tag ||= $tag;

       if( $sep eq '/')
         { 
           $perlfunc .= sprintf( _join_n(  q#foreach my $current_elt (@current_elts)              #,
                                           q#  { next if( !$current_elt);                         #,
                                           q#    $current_elt--;                                  #,
                                           q#    $elt= $stack->[$current_elt];                    #,
                                           q#    if( %s) { push @new_current_elts, $current_elt;} #,
                                           q#  }                                                  #,
                                        ),
                                 $cond
                               );
         }
       elsif( $sep eq '//')
         { 
           $perlfunc .= sprintf( _join_n(  q#foreach my $current_elt (@current_elts)                #,
                                           q#  { next if( !$current_elt);                           #,
                                           q#    $current_elt--;                                    #,
                                           q#    my $candidate= $current_elt;                       #,
                                           q#    while( $candidate >=0)                             #,
                                           q#      { $elt= $stack->[$candidate];                    #,
                                           q#        if( %s) { push @new_current_elts, $candidate;} #,
                                           q#        $candidate--;                                  #,
                                           q#      }                                                #,
                                           q#  }                                                    #,
                                        ),
                                 $cond
                               );
         }
       my $warn= $DEBUG_HANDLER >= 2 ? _join_n( qq#warn qq%fail at cond '$cond'%;#) : '';
       $perlfunc .= sprintf( _join_n( q#unless( @new_current_elts) { %s return 0; } #,
                                      q#@current_elts= @new_current_elts;           #,
                                      q#@new_current_elts=();                       #,
                                    ),
                             $warn
                           );

        $sep= pop @xpath_steps;
     }

    if( $anchored) # there should be a better way, but this works
      {  
       my $warn= $DEBUG_HANDLER >= 2 ? _join_n( qq#warn qq{fail, stack not empty};#) : '';
       $perlfunc .= sprintf( _join_n( q#if( ! grep { $_ == 0 } @current_elts) { %s return 0;}#), $warn);
      }

    $perlfunc.= qq{warn "handler for '$xpath_to_display' triggered\\n";\n} if( $DEBUG_HANDLER >=2);
    $perlfunc.= qq{return q{$xpath_original};\n};
    _warn_debug_handler( "\nperlfunc:\n$perlfunc\n") if( $DEBUG_HANDLER>=1);
    my $s= eval "sub { $perlfunc }";
      if( $@) 
        { croak "wrong handler condition '$xpath' ($@);" }

      _warn_debug_handler( "last tag: '$last_tag', test_on_text: '$flag->{test_on_text}'\n") if( $DEBUG_HANDLER >=1);
      _warn_debug_handler( "score: ", join( ' ', map { "$_: $score->{$_}" } sort keys %$score), "\n") if( $DEBUG_HANDLER >=1);
      return { tag=> $last_tag, score => $score, trigger => $s, path => $xpath_original, handler => $handler, test_on_text => $flag->{test_on_text} };
    }

sub _join_n { return join( "\n", @_, ''); }

sub _tag_cond
  { my( $full_tag)= @_;

    my( $tag, $class)= $css_sel ? $full_tag=~ m{^(.*?)(?:\.([^.]*))?$} : ($full_tag, undef);
    my $tag_cond= $tag && $tag ne '*' ? qq#(\$elt->{_tag} eq "$tag")# : '';
    my $class_cond= defined $class ? qq#(\$elt->{class}=~ m{(^| )$class( |\$)})# : '';
    my $full_cond= join( ' && ', grep { $_ } ( $tag_cond, $class_cond));
    
    return $full_cond;
  }

# input: the predicate ($_[0]) which will be changed in place
#        flags, a hashref with various flags (like test_on_text)
#        the score 
sub _parse_predicate_in_handler
  { my( $flag, $score)= @_[1..2];
    $_[0]=~ s{(   ($REG_STRING)                        # strings
                 |\@($REG_NAME)(?=\s*(?:[><=!]|!~|=~)) # @att (followed by a comparison operator)
                 |\@($REG_NAME)                        # @att (not followed by a comparison operator)
                 |=~|!~                                # matching operators
                 |([><]=?|=|!=)(?=\s*[\d+-])           # test before a number
                 |([><]=?|=|!=)                        # test, other cases
                 |($REG_FUNCTION)                      # no arg functions
                 # this bit is a mess, but it is the only solution with this half-baked parser
                 |(string\(\s*$REG_NAME\s*\)\s*$REG_MATCH\s*$REG_REGEXP)  # string( child)=~ /regexp/
                 |(string\(\s*$REG_NAME\s*\)\s*$REG_COMP\s*$REG_STRING)   # string( child) = "value" (or other test)
                 |(string\(\s*$REG_NAME\s*\)\s*$REG_COMP\s*$REG_NUMBER)   # string( child) = nb (or other test)
                 |(and|or)
                # |($REG_NAME(?=\s*(and|or|$)))         # nested tag name (needs to be after all other unquoted strings)
                 |($REG_TAG_IN_PREDICATE)              # nested tag name (needs to be after all other unquoted strings)
                 
              )}
             { my( $token, $str, $att, $bare_att, $num_test, $alpha_test, $func, $str_regexp, $str_test_alpha, $str_test_num, $and_or, $tag) 
               = ( $1,     $2,   $3,   $4,        $5,        $6,          $7,    $8,          $9,              $10,           $11,     $12); 
    
               $score->{predicates}++;
              
               # store tests on text (they are not always allowed)
               if( $func || $str_regexp || $str_test_num || $str_test_alpha ) { $flag->{test_on_text}= 1;   }

               if( defined $str)      { $token }
               elsif( $tag)           { qq{(\$elt->{_elt} && \$elt->{_elt}->has_child( '$tag'))} }
               elsif( $att)           { $att=~ m{^#} ? qq{ (\$elt->{_elt} && \$elt->{_elt}->{att}->{'$att'})}
                                                     : qq{\$elt->{'$att'}}
                                      }
                                        # for some reason Devel::Cover flags the following lines as not tested. They are though.
               elsif( $bare_att)      { $bare_att=~ m{^#} ? qq{(\$elt->{_elt} && defined(\$elt->{_elt}->{att}->{'$bare_att'}))}
                                                          : qq{defined( \$elt->{'$bare_att'})}
                                      }
               elsif( $num_test && ($num_test eq '=') ) { "==" } # others tests are unchanged
               elsif( $alpha_test)    { $PERL_ALPHA_TEST{$alpha_test} }
               elsif( $func && $func=~ m{^string})
                                      { "\$elt->{_elt}->text"; }
               elsif( $str_regexp     && $str_regexp     =~ m{string\(\s*($REG_NAME)\s*\)\s*($REG_MATCH)\s*($REG_REGEXP)})
                                      { "defined( _first_n {  \$_->text $2 $3 } 1, \$elt->{_elt}->_children( '$1'))"; }
               elsif( $str_test_alpha && $str_test_alpha =~ m{string\(\s*($REG_NAME)\s*\)\s*($REG_COMP)\s*($REG_STRING)})
                                      { my( $tag, $op, $str)= ($1, $2, $3);
                                        $str=~ s{(?<=.)'(?=.)}{\\'}g; # escape a quote within the string 
                                        $str=~ s{^"}{'};
                                        $str=~ s{"$}{'};
                                        "defined( _first_n { \$_->text $PERL_ALPHA_TEST{$op} $str } 1, \$elt->{_elt}->children( '$tag'))"; }
               elsif( $str_test_num   && $str_test_num   =~ m{string\(\s*($REG_NAME)\s*\)\s*($REG_COMP)\s*($REG_NUMBER)})
                                      { my $test= ($2 eq '=') ? '==' : $2;
                                        "defined( _first_n { \$_->text $test $3 } 1, \$elt->{_elt}->children( '$1'))"; 
                                      }
               elsif( $and_or)        { $score->{tests}++; $and_or eq 'and' ? '&&' : '||' ; }
               else                   { $token; }
             }gexs;
  }
    

sub setCharHandler
  { my( $t, $handler)= @_;
    $t->{twig_char_handler}= $handler;
  }


sub _reset_handlers
  { my $handlers= shift;
    delete $handlers->{handlers};
    delete $handlers->{path_handlers};
    delete $handlers->{subpath_handlers};
    $handlers->{attcond_handlers_exp}=[] if( $handlers->{attcond_handlers});
    delete $handlers->{attcond_handlers};
  }
  
sub _set_handlers
  { my $handlers= shift || return;
    my $set_handlers= {};
    foreach my $path (keys %{$handlers})
      { _set_handler( $set_handlers, $path, $handlers->{$path}); }
    
    return $set_handlers;
  }
    

sub setTwigHandler
  { my( $t, $path, $handler)= @_;
    $t->{twig_handlers} ||={};
    return _set_handler( $t->{twig_handlers}, $path, $handler);
  }

sub setTwigHandlers
  { my( $t, $handlers)= @_;
    my $previous_handlers= $t->{twig_handlers} || undef;
    _reset_handlers( $t->{twig_handlers});
    $t->{twig_handlers}= _set_handlers( $handlers);
    return $previous_handlers;
  }

sub setStartTagHandler
  { my( $t, $path, $handler)= @_;
    $t->{twig_starttag_handlers}||={};
    return _set_handler( $t->{twig_starttag_handlers}, $path, $handler);
  }

sub setStartTagHandlers
  { my( $t, $handlers)= @_;
    my $previous_handlers= $t->{twig_starttag_handlers} || undef;
    _reset_handlers( $t->{twig_starttag_handlers});
    $t->{twig_starttag_handlers}= _set_handlers( $handlers);
    return $previous_handlers;
   }

sub setIgnoreEltsHandler
  { my( $t, $path, $action)= @_;
    $t->{twig_ignore_elts_handlers}||={};
    return _set_handler( $t->{twig_ignore_elts_handlers}, $path, $action );
  }

sub setIgnoreEltsHandlers
  { my( $t, $handlers)= @_;
    my $previous_handlers= $t->{twig_ignore_elts_handlers};
    _reset_handlers( $t->{twig_ignore_elts_handlers});
    $t->{twig_ignore_elts_handlers}= _set_handlers( $handlers);
    return $previous_handlers;
   }

sub setEndTagHandler
  { my( $t, $path, $handler)= @_;
    $t->{twig_endtag_handlers}||={};
    return _set_handler( $t->{twig_endtag_handlers}, $path,$handler);
  }

sub setEndTagHandlers
  { my( $t, $handlers)= @_;
    my $previous_handlers= $t->{twig_endtag_handlers};
    _reset_handlers( $t->{twig_endtag_handlers});
    $t->{twig_endtag_handlers}= _set_handlers( $handlers);
    return $previous_handlers;
   }

# a little more complex: set the twig_handlers only if a code ref is given
sub setTwigRoots
  { my( $t, $handlers)= @_;
    my $previous_roots= $t->{twig_roots};
    _reset_handlers($t->{twig_roots});
    $t->{twig_roots}= _set_handlers( $handlers);

    _check_illegal_twig_roots_handlers( $t->{twig_roots});
    
    foreach my $path (keys %{$handlers})
      { $t->{twig_handlers}||= {};
        _set_handler( $t->{twig_handlers}, $path, $handlers->{$path})
          if( ref($handlers->{$path}) && isa( $handlers->{$path}, 'CODE')); 
      }
    return $previous_roots;
  }

sub _check_illegal_twig_roots_handlers
  { my( $handlers)= @_;
    foreach my $tag_handlers (values %{$handlers->{xpath_handler}})
      { foreach my $handler_data (@$tag_handlers)
          { if( my $type= $handler_data->{test_on_text})
              { croak "string() condition not supported on twig_roots option"; }
          }
      }
    return;
  }
    

# just store the reference to the expat object in the twig
sub _twig_init
   { # warn " in _twig_init...\n"; # DEBUG handler
    
    my $p= shift;
    my $t=$p->{twig};

    if( $t->{twig_parsing} ) { croak "cannot reuse a twig that is already parsing"; }
    $t->{twig_parsing}=1;

    $t->{twig_parser}= $p; 
    if( $weakrefs) { weaken( $t->{twig_parser}); }

    # in case they had been created by a previous parse
    delete $t->{twig_dtd};
    delete $t->{twig_doctype};
    delete $t->{twig_xmldecl};
    delete $t->{twig_root};

    # if needed set the output filehandle
    $t->_set_fh_to_twig_output_fh();
    return;
  }

# uses eval to catch the parser's death
sub safe_parse
  { my $t= shift;
    eval { $t->parse( @_); } ;
    return $@ ? $t->_reset_twig_after_error : $t;
  }

sub safe_parsefile
  { my $t= shift;
    eval { $t->parsefile( @_); } ;
    return $@ ? $t->_reset_twig_after_error : $t;
  }

# appdynamicsore a twig in a proper state so it can be reused for a new parse
sub _reset_twig
  { my $t= shift;
    $t->{twig_parsing}= 0;
    delete $t->{twig_current};
    delete $t->{extra_data};
    delete $t->{twig_dtd};
    delete $t->{twig_in_pcdata};
    delete $t->{twig_in_cdata};
    delete $t->{twig_stored_space};
    delete $t->{twig_entity_list};
    $t->root->delete if( $t->root);
    delete $t->{root};
    return $t;
  }

sub _reset_twig_after_error
  { my $t= shift;
    $t->_reset_twig;
    return undef;
  }


sub _add_or_discard_stored_spaces
  { my $t= shift;
   
    $t->{twig_right_after_root}=0; #XX

    my $current= $t->{twig_current} or return; # ugly hack, with ignore on, twig_current can disappear 
    if( $t->{twig_stored_spaces} || $t->{twig_preserve_space})
      { if( (exists $current->{'pcdata'}))
          { $current->{pcdata}.= $t->{twig_stored_spaces}; }
        else
          { my $current_gi= $XML::Twig::index2gi[$current->{'gi'}];

            if( $t->{twig_discard_all_spaces}) { $t->{twig_stored_spaces}=''; return; }

            if( ! defined( $t->{twig_space_policy}->{$current_gi}))
              { $t->{twig_space_policy}->{$current_gi}= _space_policy( $t, $current_gi); }

            if(    $t->{twig_space_policy}->{$current_gi} ||  ($t->{twig_stored_spaces}!~ m{\n})
                || $t->{twig_preserve_space}
              )
              { _insert_pcdata( $t, $t->{twig_stored_spaces} ); }
            $t->{twig_stored_spaces}='';

          }
      }
    return;
  }

# the default twig handlers, which build the tree
sub _twig_start
   { # warn " in _twig_start...\n"; # DEBUG handler
    
    #foreach my $s (@_) { next if ref $s; warn "$s: ", is_utf8( $s) ? "has flag" : "FLAG NOT SET"; } # YYY

    my ($p, $gi, @att)= @_;
    my $t=$p->{twig};

    # empty the stored pcdata (space stored in case they are really part of 
    # a pcdata element) or stored it if the space policy dictades so
    # create a pcdata element with the spaces if need be
    _add_or_discard_stored_spaces( $t);
    my $parent= $t->{twig_current};

    # if we were parsing PCDATA then we exit the pcdata
    if( $t->{twig_in_pcdata})
      { $t->{twig_in_pcdata}= 0;
        delete $parent->{'twig_current'};
        $parent= $parent->{parent};
      }

    # if we choose to keep the encoding then we need to parse the tag
    if( my $func = $t->{parse_start_tag})
      { ($gi, @att)= &$func($p->original_string); }
    elsif( $t->{twig_entities_in_attribute})
      { 
       ($gi,@att)= _parse_start_tag( $p->recognized_string); 
         $t->{twig_entities_in_attribute}=0;
      }

    # if we are using an external DTD, we need to fill the default attributes
    if( $t->{twig_read_external_dtd}) { _fill_default_atts( $t, $gi, \@att); }
    
    # filter the input data if need be  
    if( my $filter= $t->{twig_input_filter})
      { $gi= $filter->( $gi);
        foreach my $att (@att) { $att= $filter->($att); } 
      }

    if( $t->{twig_map_xmlns}) { _replace_ns( $t, \$gi, \@att); }

    my $elt= $t->{twig_elt_class}->new( $gi);
    $elt->set_atts( @att);
 
    # now we can store the tag and atts
    my $context= { _tag => $gi, _elt => $elt, @att};
    if( $weakrefs) { weaken( $context->{_elt}); }
    push @{$t->{_twig_context_stack}}, $context;

    delete $parent->{'twig_current'} if( $parent);
    $t->{twig_current}= $elt;
    $elt->{'twig_current'}=1;

    if( $parent)
      { my $prev_sibling= $parent->{last_child};
        if( $prev_sibling) 
          { $prev_sibling->{next_sibling}=  $elt; 
            $elt->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;
          }

        $elt->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
        unless( $parent->{first_child}) { $parent->{first_child}=  $elt; } 
         $parent->{empty}=0; $parent->{last_child}=$elt; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ;
      }
    else 
      { # processing root
        $t->set_root( $elt);
        # call dtd handler if need be
        $t->{twig_dtd_handler}->($t, $t->{twig_dtd})
          if( defined $t->{twig_dtd_handler});
      
        # set this so we can catch external entities
        # (the handler was modified during DTD processing)
        if( $t->{twig_default_print})
          { $p->setHandlers( Default => \&_twig_print); }
        elsif( $t->{twig_roots})
          { $p->setHandlers( Default => sub { return }); }
        else
          { $p->setHandlers( Default => \&_twig_default); }
      }
  
    $elt->{empty}=  $p->recognized_string=~ m{/\s*>$}s ? 1 : 0;

    $elt->{extra_data}= $t->{extra_data} if( $t->{extra_data});
    $t->{extra_data}='';

    # if the element is ID-ed then store that info
    my $id= $elt->{'att'}->{$ID};
    if( defined $id)
      { $t->{twig_id_list}->{$id}= $elt; 
        if( $weakrefs) { weaken( $t->{twig_id_list}->{$id}); }
      }

    # call user handler if need be
    if( $t->{twig_starttag_handlers})
      { # call all appropriate handlers
        my @handlers= _handler( $t, $t->{twig_starttag_handlers}, $gi);
    
        local $_= $elt;
    
        foreach my $handler ( @handlers)
          { $handler->($t, $elt) || last; }
        # call _all_ handler if needed
        if( my $all= $t->{twig_starttag_handlers}->{handlers}->{$ALL})
          { $all->($t, $elt); }
      }

    # check if the tag is in the list of tags to be ignored
    if( $t->{twig_ignore_elts_handlers})
      { my @handlers= _handler( $t, $t->{twig_ignore_elts_handlers}, $gi);
        # only the first handler counts, it contains the action (discard/print/string)
        if( @handlers) { my $action= shift @handlers; $t->ignore( $elt, $action); }
      }

    if( $elt->{'att'}->{'xml:space'} && (  $elt->{'att'}->{'xml:space'} eq 'preserve')) { $t->{twig_preserve_space}++; }
    

    return;
  }

sub _replace_ns
  { my( $t, $gi, $atts)= @_;
    foreach my $new_prefix ( $t->parser->new_ns_prefixes)
      { my $uri= $t->parser->expand_ns_prefix( $new_prefix);
        # replace the prefix if it is mapped
        if( !$t->{twig_keep_original_prefix} && (my $mapped_prefix= $t->{twig_map_xmlns}->{$uri}))
          { $new_prefix= $mapped_prefix; }
        # now put the namespace declaration back in the element
        if( $new_prefix eq '#default')
          { push @$atts, "xmlns" =>  $uri; } 
        else
          { push @$atts, "xmlns:$new_prefix" =>  $uri; } 
      }

    if( $t->{twig_keep_original_prefix})
      { # things become more complex: we need to find the original prefix
        # and store both prefixes
        my $ns_info= $t->_ns_info( $$gi);
        my $map_att;
        if( $ns_info->{mapped_prefix})
          { $$gi= "$ns_info->{mapped_prefix}:$$gi";
            $map_att->{$ns_info->{mapped_prefix}}= $ns_info->{prefix};
          }
        my $att_name=1;
        foreach( @$atts) 
          { if( $att_name) 
              { 
                my $ns_info= $t->_ns_info( $_);
                if( $ns_info->{mapped_prefix})
                  { $_= "$ns_info->{mapped_prefix}:$_";
                    $map_att->{$ns_info->{mapped_prefix}}= $ns_info->{prefix};
                  }
                $att_name=0; 
              }
            else           
              {  $att_name=1; }
          }
        push @$atts, '#original_gi', $map_att if( $map_att);
      }
    else
      { $$gi= $t->_replace_prefix( $$gi); 
        my $att_name=1;
        foreach( @$atts) 
          { if( $att_name) { $_= $t->_replace_prefix( $_); $att_name=0; }
            else           {  $att_name=1; }
          }
      }
    return;
  }


# extract prefix, local_name, uri, mapped_prefix from a name
# will only work if called from a start or end tag handler
sub _ns_info
  { my( $t, $name)= @_;
    my $ns_info={};
    my $p= $t->parser;
    $ns_info->{uri}= $p->namespace( $name); 
    return $ns_info unless( $ns_info->{uri});

    $ns_info->{prefix}= _a_proper_ns_prefix( $p, $ns_info->{uri});
    $ns_info->{mapped_prefix}= $t->{twig_map_xmlns}->{$ns_info->{uri}} || $ns_info->{prefix};

    return $ns_info;
  }
    
sub _a_proper_ns_prefix
  { my( $p, $uri)= @_;
    foreach my $prefix ($p->current_ns_prefixes)
      { if( $p->expand_ns_prefix( $prefix) eq $uri)
          { return $prefix; }
      }
    return;
  }

sub _fill_default_atts
  { my( $t, $gi, $atts)= @_;
    my $dtd= $t->{twig_dtd};
    my $attlist= $dtd->{att}->{$gi};
    my %value= @$atts;
    foreach my $att (keys %$attlist)
      { if(   !exists( $value{$att}) 
            && exists( $attlist->{$att}->{default})
            && ( $attlist->{$att}->{default} ne '#IMPLIED')
          )
          { # the quotes are included in the default, so we need to remove them
            my $default_value= substr( $attlist->{$att}->{default}, 1, -1);
            push @$atts, $att, $default_value;
          }
      }
    return;
  }


# the default function to parse a start tag (in keep_encoding mode)
# can be overridden with the parse_start_tag method
# only works for 1-byte character sets
sub _parse_start_tag
  { my $string= shift;
    my( $gi, @atts);

    # get the gi (between < and the first space, / or > character)
    #if( $string=~ s{^<\s*([^\s>/]*)[\s>/]*}{}s)
    if( $string=~ s{^<\s*($REG_NAME)\s*[\s>/]}{}s)
      { $gi= $1; }
    else
      { croak "error parsing tag '$string'"; }
    while( $string=~ s{^([^\s=]*)\s*=\s*(["'])(.*?)\2\s*}{}s)
      { push @atts, $1, $3; }
    return $gi, @atts;
  }

sub set_root
  { my( $t, $elt)= @_;
    $t->{twig_root}= $elt;
    $elt->{twig}= $t;
    if( $weakrefs) { weaken(  $elt->{twig}); }
    return $t;
  }

sub _twig_end
   { # warn " in _twig_end...\n"; # DEBUG handler
    my ($p, $gi)  = @_;

    my $t=$p->{twig};

    if( $t->{twig_map_xmlns}) { $gi= $t->_replace_prefix( $gi); }
  
    _add_or_discard_stored_spaces( $t);
 
    # the new twig_current is the parent
    my $elt= $t->{twig_current};
    delete $elt->{'twig_current'};

    # if we were parsing PCDATA then we exit the pcdata too
    if( $t->{twig_in_pcdata})
      { $t->{twig_in_pcdata}= 0;
        $elt= $elt->{parent} if($elt->{parent});
        delete $elt->{'twig_current'};
      }

    # parent is the new current element
    my $parent= $elt->{parent};
    $t->{twig_current}= $parent;

    if( $parent)
      { $parent->{'twig_current'}=1;
        # twig_to_be_normalized
        if( $parent->{twig_to_be_normalized}) { $parent->normalize; $parent->{twig_to_be_normalized}=0; }
      }

    if( $t->{extra_data}) 
      { $elt->_set_extra_data_before_end_tag( $t->{extra_data});  
        $t->{extra_data}='';
      }

    if( $t->{twig_handlers})
      { # look for handlers
        my @handlers= _handler( $t, $t->{twig_handlers}, $gi);
        
        if( $t->{twig_tdh})
          { if( @handlers) { push @{$t->{twig_handlers_to_trigger}}, [ $elt, \@handlers ]; }
            if( my $all= $t->{twig_handlers}->{handlers}->{$ALL}) 
              { push @{$t->{twig_handlers_to_trigger}}, [ $elt, [$all] ]; }
          }
        else
          {
            local $_= $elt; # so we can use $_ in the handlers
    
            foreach my $handler ( @handlers)
              { $handler->($t, $elt) || last; }
            # call _all_ handler if needed
            my $all= $t->{twig_handlers}->{handlers}->{$ALL};
            if( $all)
              { $all->($t, $elt); }
            if( @handlers || $all) { $t->{twig_right_after_root}=0; }
          }
      }

    # if twig_roots is set for the element then set appropriate handler
    if(  $t->{twig_root_depth} and ($p->depth == $t->{twig_root_depth}) )
      { if( $t->{twig_default_print})
          { # select the proper fh (and store the currently selected one)
            $t->_set_fh_to_twig_output_fh(); 
            if( !$p->depth==1) { $t->{twig_right_after_root}=1; } #XX
            if( $t->{twig_keep_encoding})
              { $p->setHandlers( %twig_handlers_roots_print_original); }
            else
              { $p->setHandlers( %twig_handlers_roots_print); }
          }
        else
          { $p->setHandlers( %twig_handlers_roots); }
      }

    if( $elt->{'att'}->{'xml:space'} && (  $elt->{'att'}->{'xml:space'} eq 'preserve')) { $t->{twig_preserve_space}--; }

    pop @{$t->{_twig_context_stack}};
    return;
  }

sub _trigger_tdh
  { my( $t)= @_;

    if( @{$t->{twig_handlers_to_trigger}})
      { my @handlers_to_trigger_now= sort { $a->[0]->cmp( $b->[0]) } @{$t->{twig_handlers_to_trigger}};
        foreach my $elt_handlers (@handlers_to_trigger_now)
          { my( $handled_elt, $handlers_to_trigger)= @$elt_handlers;
            foreach my $handler ( @$handlers_to_trigger) 
              { local $_= $handled_elt; $handler->($t, $handled_elt) || last; }
          }
      }
    return;
  }

# return the list of handler that can be activated for an element 
# (either of CODE ref's or 1's for twig_roots)

sub _handler
  { my( $t, $handlers, $gi)= @_;

    my @found_handlers=();
    my $found_handler;

    foreach my $handler ( map { @$_ } grep { $_ } $handlers->{xpath_handler}->{$gi}, $handlers->{xpath_handler}->{'*'})
      {  my $trigger= $handler->{trigger};
         if( my $found_path= $trigger->( $t->{_twig_context_stack}))
          { my $found_handler= $handler->{handler};
            push @found_handlers, $found_handler; 
          }
      }

    # if no handler found call default handler if defined
    if( !@found_handlers && defined $handlers->{handlers}->{$DEFAULT})
      { push @found_handlers, $handlers->{handlers}->{$DEFAULT}; }

    if( @found_handlers and $t->{twig_do_not_chain_handlers}) 
      { @found_handlers= ($found_handlers[0]); }

    return @found_handlers; # empty if no handler found

  }


sub _replace_prefix
  { my( $t, $name)= @_;
    my $p= $t->parser;
    my $uri= $p->namespace( $name);
    # try to get the namespace from default if none is found (for attributes)
    # this should probably be an option
    if( !$uri and( $name!~/^xml/)) { $uri= $p->expand_ns_prefix( '#default'); }
    if( $uri)
      { if (my $mapped_prefix= $t->{twig_map_xmlns}->{$uri} || $DEFAULT_URI2NS{$uri})
          { return "$mapped_prefix:$name"; }
        else
          { my $prefix= _a_proper_ns_prefix( $p, $uri);
            if( $prefix eq '#default') { $prefix=''; }
            return $prefix ? "$prefix:$name" : $name; 
          }
      }
    else
      { return $name; }
  }


sub _twig_char
   { # warn " in _twig_char...\n"; # DEBUG handler
    
    my ($p, $string)= @_;
    my $t=$p->{twig}; 

    if( $t->{twig_keep_encoding})
      { if( !$t->{twig_in_cdata})
          { $string= $p->original_string(); }
        else
          { 
            use bytes; # > perl 5.5
            if( length( $string) < 1024)
              { $string= $p->original_string(); }
            else
              { #warn "dodgy case";
                # TODO original_string does not hold the entire string, but $string is wrong
                # I believe due to a bug in XML::Parser
                # for now, we use the original string, even if it means that it's been converted to utf8
              }
          }
      }

    if( $t->{twig_input_filter}) { $string= $t->{twig_input_filter}->( $string); }
    if( $t->{twig_char_handler}) { $string= $t->{twig_char_handler}->( $string); }

    my $elt= $t->{twig_current};

    if(    $t->{twig_in_cdata})
      { # text is the continuation of a previously created cdata
        $elt->{cdata}.=  $t->{twig_stored_spaces} . $string;
      } 
    elsif( $t->{twig_in_pcdata})
      { # text is the continuation of a previously created pcdata
        if( $t->{extra_data})
          { $elt->_push_extra_data_in_pcdata( $t->{extra_data}, length( $elt->{pcdata}));
            $t->{extra_data}='';
          }
        $elt->{pcdata}.=  $string; 
      } 
    else
      { 
        # text is just space, which might be discarded later
        if( $string=~/\A\s*\Z/s)
          { 
            if( $t->{extra_data})
              { # we got extra data (comment, pi), lets add the spaces to it
                $t->{extra_data} .= $string; 
              }
            else
              { # no extra data, just store the spaces
                $t->{twig_stored_spaces}.= $string;
              }
          } 
        else
          { my $new_elt= _insert_pcdata( $t, $t->{twig_stored_spaces}.$string);
            delete $elt->{'twig_current'};
            $new_elt->{'twig_current'}=1;
            $t->{twig_current}= $new_elt;
            $t->{twig_in_pcdata}=1;
            if( $t->{extra_data})
              { $new_elt->_push_extra_data_in_pcdata( $t->{extra_data}, 0);
                $t->{extra_data}='';
              }
          }
      }
    return; 
  }

sub _twig_cdatastart
   { # warn " in _twig_cdatastart...\n"; # DEBUG handler
    
    my $p= shift;
    my $t=$p->{twig};

    $t->{twig_in_cdata}=1;
    my $cdata=  $t->{twig_elt_class}->new( $CDATA);
    my $twig_current= $t->{twig_current};

    if( $t->{twig_in_pcdata})
      { # create the node as a sibling of the PCDATA
        $cdata->{prev_sibling}=$twig_current; if( $XML::Twig::weakrefs) { weaken( $cdata->{prev_sibling});} ;
        $twig_current->{next_sibling}=  $cdata;
        my $parent= $twig_current->{parent};
        $cdata->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $cdata->{parent});} ;
         $parent->{empty}=0; $parent->{last_child}=$cdata; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ;
        $t->{twig_in_pcdata}=0;
      }
    else
      { # we have to create a PCDATA element if we need to store spaces
        if( $t->_space_policy($XML::Twig::index2gi[$twig_current->{'gi'}]) && $t->{twig_stored_spaces})
          { _insert_pcdata( $t, $t->{twig_stored_spaces}); }
        $t->{twig_stored_spaces}='';
      
        # create the node as a child of the current element      
        $cdata->{parent}=$twig_current; if( $XML::Twig::weakrefs) { weaken( $cdata->{parent});} ;
        if( my $prev_sibling= $twig_current->{last_child})
          { $cdata->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $cdata->{prev_sibling});} ;
            $prev_sibling->{next_sibling}=  $cdata;
          }
        else
          { $twig_current->{first_child}=  $cdata; }
         $twig_current->{empty}=0; $twig_current->{last_child}=$cdata; if( $XML::Twig::weakrefs) { weaken( $twig_current->{last_child});} ;
      
      }

    delete $twig_current->{'twig_current'};
    $t->{twig_current}= $cdata;
    $cdata->{'twig_current'}=1;
    if( $t->{extra_data}) { $cdata->set_extra_data( $t->{extra_data}); $t->{extra_data}='' };
    return;
  }

sub _twig_cdataend
   { # warn " in _twig_cdataend...\n"; # DEBUG handler
    
    my $p= shift;
    my $t=$p->{twig};

    $t->{twig_in_cdata}=0;

    my $elt= $t->{twig_current};
    delete $elt->{'twig_current'};
    my $cdata= $elt->{cdata};
    $elt->_set_cdata( $cdata);

    push @{$t->{_twig_context_stack}}, { _tag => $CDATA };

    if( $t->{twig_handlers})
      { # look for handlers
        my @handlers= _handler( $t, $t->{twig_handlers}, $CDATA);
        local $_= $elt; # so we can use $_ in the handlers
        foreach my $handler ( @handlers) { $handler->($t, $elt) || last; }
      }

    pop @{$t->{_twig_context_stack}};

    $elt= $elt->{parent};
    $t->{twig_current}= $elt;
    $elt->{'twig_current'}=1;

    $t->{twig_long_cdata}=0;
    return;
  }

sub _pi_elt_handlers
  { my( $t, $pi)= @_;
    my $pi_handlers= $t->{twig_handlers}->{pi_handlers} || return;
    foreach my $handler ( $pi_handlers->{$pi->{target}}, $pi_handlers->{''})
      { if( $handler) { local $_= $pi; $handler->( $t, $pi) || last; } }
  }

sub _pi_text_handler
  { my( $t, $target, $data)= @_;
    if( my $handler= $t->{twig_handlers}->{pi_handlers}->{$target})
      { return $handler->( $t, $target, $data); }
    if( my $handler= $t->{twig_handlers}->{pi_handlers}->{''})
      { return $handler->( $t, $target, $data); }
    return defined( $data) && $data ne ''  ? "<?$target $data?>" : "<?$target?>" ;
  }

sub _comment_elt_handler
  { my( $t, $comment)= @_; 
    if( my $handler= $t->{twig_handlers}->{handlers}->{$COMMENT})
      { local $_= $comment; $handler->($t, $comment); }
  }

sub _comment_text_handler
  { my( $t, $comment)= @_; 
    if( my $handler= $t->{twig_handlers}->{handlers}->{$COMMENT})
      { $comment= $handler->($t, $comment); 
        if( !defined $comment || $comment eq '') { return ''; }
      }
    return "<!--$comment-->";
  }



sub _twig_comment
   { # warn " in _twig_comment...\n"; # DEBUG handler
    
    my( $p, $comment_text)= @_;
    my $t=$p->{twig};

    if( $t->{twig_keep_encoding}) { $comment_text= substr( $p->original_string(), 4, -3); }
    
    $t->_twig_pi_comment( $p, $COMMENT, $t->{twig_keep_comments}, $t->{twig_process_comments},
                          '_set_comment', '_comment_elt_handler', '_comment_text_handler', $comment_text
                        );
    return;
  }

sub _twig_pi
   { # warn " in _twig_pi...\n"; # DEBUG handler
    
    my( $p, $target, $data)= @_;
    my $t=$p->{twig};

    if( $t->{twig_keep_encoding}) 
      { my $pi_text= substr( $p->original_string(), 2, -2); 
        ($target, $data)= split( /\s+/, $pi_text, 2);
      }

    $t->_twig_pi_comment( $p, $PI, $t->{twig_keep_pi}, $t->{twig_process_pi},
                          '_set_pi', '_pi_elt_handlers', '_pi_text_handler', $target, $data
                        );
    return;
  }

sub _twig_pi_comment
  { my( $t, $p, $type, $keep, $process, $set, $elt_handler, $text_handler, @parser_args)= @_;

    if( $t->{twig_input_filter})
          { foreach my $arg (@parser_args) { $arg= $t->{twig_input_filter}->( $arg); } }
          
    # if pi/comments are to be kept then we piggiback them to the current element
    if( $keep)
      { # first add spaces
        if( $t->{twig_stored_spaces})
              { $t->{extra_data}.= $t->{twig_stored_spaces};
                $t->{twig_stored_spaces}= '';
              }

        my $extra_data= $t->$text_handler( @parser_args);
        $t->{extra_data}.= $extra_data;

      }
    elsif( $process)
      {
        my $twig_current= $t->{twig_current}; # defined unless we are outside of the root

        my $elt= $t->{twig_elt_class}->new( $type);
        $elt->$set( @parser_args);
        if( $t->{extra_data}) 
          { $elt->set_extra_data( $t->{extra_data});
            $t->{extra_data}='';
          }

        unless( $t->root) 
          { $t->_add_cpi_outside_of_root( leading_cpi => $elt);
          }
        elsif( $t->{twig_in_pcdata})
          { # create the node as a sibling of the PCDATA
            $elt->paste_after( $twig_current);
            $t->{twig_in_pcdata}=0;
          }
        elsif( $twig_current)
          { # we have to create a PCDATA element if we need to store spaces
            if( $t->_space_policy($XML::Twig::index2gi[$twig_current->{'gi'}]) && $t->{twig_stored_spaces})
              { _insert_pcdata( $t, $t->{twig_stored_spaces}); }
            $t->{twig_stored_spaces}='';
            # create the node as a child of the current element
            $elt->paste_last_child( $twig_current);
          }
        else
          { $t->_add_cpi_outside_of_root( trailing_cpi => $elt); }

        if( $twig_current)
          { delete $twig_current->{'twig_current'};
            my $parent= $elt->{parent};
            $t->{twig_current}= $parent;
            $parent->{'twig_current'}=1;
          }

        $t->$elt_handler( $elt);
      }

  }
    

# add a comment or pi before the first element
sub _add_cpi_outside_of_root
  { my($t, $type, $elt)= @_; # $type is 'leading_cpi' or 'trailing_cpi'
    $t->{$type} ||= $t->{twig_elt_class}->new( '#CPI');
    # create the node as a child of the current element
    $elt->paste_last_child( $t->{$type});
    return $t;
  }
  
sub _twig_final
   { # warn " in _twig_final...\n"; # DEBUG handler
    
    my $p= shift;
    my $t= $p->isa( 'XML::Twig') ? $p : $p->{twig};

    # store trailing data
    if( $t->{extra_data}) { $t->{trailing_cpi_text} = $t->{extra_data}; $t->{extra_data}=''; }
    $t->{trailing_spaces}= $t->{twig_stored_spaces} || ''; 
    my $s=  $t->{twig_stored_spaces}; $s=~s{\n}{\\n}g;
    if( $t->{twig_stored_spaces}) { my $s=  $t->{twig_stored_spaces}; }

    # appdynamicsore the selected filehandle if needed
    $t->_set_fh_to_selected_fh();

    $t->_trigger_tdh if( $t->{twig_tdh});

    select $t->{twig_original_selected_fh} if($t->{twig_original_selected_fh}); # probably dodgy

    if( exists $t->{twig_autoflush_data})
      { my @args;
        push @args,  $t->{twig_autoflush_data}->{fh}      if( $t->{twig_autoflush_data}->{fh});
        push @args,  @{$t->{twig_autoflush_data}->{args}} if( $t->{twig_autoflush_data}->{args});
        $t->flush( @args);
        delete $t->{twig_autoflush_data};
        $t->root->delete;
      }

    # tries to clean-up (probably not very well at the moment)
    #undef $p->{twig};
    undef $t->{twig_parser};
    delete $t->{twig_parsing};
    @{$t}{ qw( twig_parser twig_parsing _twig_context_stack twig_current) }=();

    return $t;
  }

sub _insert_pcdata
  { my( $t, $string)= @_;
    # create a new PCDATA element
    my $parent= $t->{twig_current};    # always defined
    my $elt;
    if( exists $t->{twig_alt_elt_class})
      { $elt=  $t->{twig_elt_class}->new( $PCDATA);
        $elt->_set_pcdata( $string);
      }
    else
      { $elt= bless( { gi => $XML::Twig::gi2index{$PCDATA}, pcdata => $string }, 'XML::Twig::Elt'); }

    my $prev_sibling= $parent->{last_child};
    if( $prev_sibling) 
      { $prev_sibling->{next_sibling}=  $elt; 
        $elt->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;
      }
    else
      { $parent->{first_child}=  $elt; }

    $elt->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
     $parent->{empty}=0; $parent->{last_child}=$elt; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ;
    $t->{twig_stored_spaces}='';
    return $elt;
  }

sub _space_policy
  { my( $t, $gi)= @_;
    my $policy;
    $policy=0 if( $t->{twig_discard_spaces});
    $policy=1 if( $t->{twig_keep_spaces});
    $policy=1 if( $t->{twig_keep_spaces_in}
               && $t->{twig_keep_spaces_in}->{$gi});
    $policy=0 if( $t->{twig_discard_spaces_in} 
               && $t->{twig_discard_spaces_in}->{$gi});
    return $policy;
  }


sub _twig_entity
   { # warn " in _twig_entity...\n"; # DEBUG handler
    my( $p, $name, $val, $sysid, $pubid, $ndata, $param)= @_;
    my $t=$p->{twig};

    #{ no warnings; my $base= $p->base; warn "_twig_entity called: expand: '$t->{twig_expand_external_ents}', base: '$base', name: '$name', val: '$val', sysid: '$sysid', pubid: '$pubid', ndata: '$ndata', param: '$param'\n";}

    my $missing_entity=0;

    if( $sysid) 
      { if($ndata)
          { if( ! -f _based_filename( $sysid, $p->base)) { $missing_entity= 1; }
          }
        else
          { if( $t->{twig_expand_external_ents})
              { $val= eval { _slurp_uri( $sysid, $p->base) };
                if( ! defined $val) 
                  { if( $t->{twig_extern_ent_nofail}) 
                      { $missing_entity= 1; }
                    else
                      { _croak( "cannot load SYSTEM entity '$name' from '$sysid': $@", 3); }
                  }
              }
          }
      }

    my $ent=XML::Twig::Entity->new( $name, $val, $sysid, $pubid, $ndata, $param);
    if( $missing_entity) { $t->{twig_missing_system_entities}->{$name}= $ent; }

    my $entity_list= $t->entity_list;
    if( $entity_list) { $entity_list->add( $ent); }

    if( $parser_version > 2.27) 
      { # this is really ugly, but with some versions of XML::Parser the value 
        # of the entity is not properly returned by the default handler
        my $ent_decl= $ent->text;
        if( $t->{twig_keep_encoding})
          { if( defined $ent->{val} && ($ent_decl !~ /["']/))
              { my $val=  $ent->{val};
                $ent_decl .= $val =~ /"/ ? qq{'$val' } : qq{"$val" }; 
              }
            # for my solaris box (perl 5.6.1, XML::Parser 2.31, expat?)
            $t->{twig_doctype}->{internal}=~ s{<!ENTITY\s+$name\s+$}{substr( $ent_decl, 0, -1)}e;
          }
        $t->{twig_doctype}->{internal} .= $ent_decl 
          unless( $t->{twig_doctype}->{internal}=~ m{<!ENTITY\s+$name\s+});
      }

    return;
  }


sub _twig_extern_ent
   { # warn " in _twig_extern_ent...I (", $_[0]->original_string, ")\n"; # DEBUG handler
    my( $p, $base, $sysid, $pubid)= @_;
    my $t= $p->{twig};
    if( $t->{twig_no_expand}) 
      { my $ent_name= $t->{twig_keep_encoding} ? $p->original_string : $p->recognized_string;
        _twig_insert_ent( $t, $ent_name);
        return '';
      }
    my $ent_content= eval { $t->{twig_ext_ent_handler}->( $p, $base, $sysid) };
    if( ! defined $ent_content)
      { 
        my $ent_name = $p->recognized_string;
        my $file     =  _based_filename( $sysid, $base);
        my $error_message= "cannot expand $ent_name - cannot load '$file'";
        if( $t->{twig_extern_ent_nofail}) { return "<!-- $error_message -->"; }
        else                              { _croak( $error_message);   }
      }
    return $ent_content; 
  }

# I use this so I can change the $Carp::CarpLevel (which determines how many call frames to skip when reporting an error)
sub _croak
  { my( $message, $level)= @_;
    $Carp::CarpLevel= $level || 0;
    croak $message;
  }

sub _twig_xmldecl
   { # warn " in _twig_xmldecl...\n"; # DEBUG handler
    
    my $p= shift;
    my $t=$p->{twig};
    $t->{twig_xmldecl}||={};                 # could have been set by set_output_encoding
    $t->{twig_xmldecl}->{version}= shift;
    $t->{twig_xmldecl}->{encoding}= shift; 
    $t->{twig_xmldecl}->{standalone}= shift;
    return;
  }

sub _twig_doctype
   { # warn " in _twig_doctype...\n"; # DEBUG handler
    my( $p, $name, $sysid, $pub, $internal)= @_;
    my $t=$p->{twig};
    $t->{twig_doctype}||= {};                   # create 
    $t->{twig_doctype}->{name}= $name;          # always there
    $t->{twig_doctype}->{sysid}= $sysid;        #  
    $t->{twig_doctype}->{pub}= $pub;            #  

    # now let's try to cope with XML::Parser 2.28 and above
    if( $parser_version > 2.27)
      { @saved_default_handler= $p->setHandlers( Default     => \&_twig_store_internal_dtd,
                                                 Entity      => \&_twig_entity,
                                               );
      $p->setHandlers( DoctypeFin  => \&_twig_stop_storing_internal_dtd);
      $t->{twig_doctype}->{internal}='';
      }
    else            
      # for XML::Parser before 2.28
      { $internal||='';
        $internal=~ s{^\s*\[}{}; 
        $internal=~ s{]\s*$}{}; 
        $t->{twig_doctype}->{internal}=$internal; 
      }

    # now check if we want to get the DTD info
    if( $t->{twig_read_external_dtd} && $sysid)
      { # let's build a fake document with an internal DTD
        my $dtd=  "<!DOCTYPE $name [" . _slurp_uri( $sysid) .  "]><$name/>";
       
        $t->save_global_state();            # save the globals (they will be reset by the following new)  
        my $t_dtd= XML::Twig->new( load_DTD => 1, ParseParamEnt => 1, error_context => $t->{ErrorContext} || 0);          # create a temp twig
        $t_dtd->parse( $dtd);               # parse it
        $t->{twig_dtd}= $t_dtd->{twig_dtd}; # grab the dtd info
        #$t->{twig_dtd_is_external}=1;
        $t->entity_list->_add_list( $t_dtd->entity_list) if( $t_dtd->entity_list); # grab the entity info
        $t->appdynamicsore_global_state();
      }
    return;
  }

sub _twig_element
   { # warn " in _twig_element...\n"; # DEBUG handler
    
    my( $p, $name, $model)= @_;
    my $t=$p->{twig};
    $t->{twig_dtd}||= {};                      # may create the dtd 
    $t->{twig_dtd}->{model}||= {};             # may create the model hash 
    $t->{twig_dtd}->{elt_list}||= [];          # ordered list of elements 
    push @{$t->{twig_dtd}->{elt_list}}, $name; # store the elt
    $t->{twig_dtd}->{model}->{$name}= $model;  # store the model
    if( ($parser_version > 2.27) && ($t->{twig_doctype}->{internal}=~ m{(^|>)\s*$}) ) 
      { my $text= $XML::Twig::Elt::keep_encoding ? $p->original_string : $p->recognized_string; 
        unless( $text)
          { # this version of XML::Parser does not return the text in the *_string method
            # we need to rebuild it
            $text= "<!ELEMENT $name $model>";
          }
        $t->{twig_doctype}->{internal} .= $text;
      }
    return;
  }

sub _twig_attlist
   { # warn " in _twig_attlist...\n"; # DEBUG handler
    
    my( $p, $gi, $att, $type, $default, $fixed)= @_;
    #warn "in attlist: gi: '$gi', att: '$att', type: '$type', default: '$default', fixed: '$fixed'\n";
    my $t=$p->{twig};
    $t->{twig_dtd}||= {};                      # create dtd if need be 
    $t->{twig_dtd}->{$gi}||= {};               # create elt if need be 
    #$t->{twig_dtd}->{$gi}->{att}||= {};        # create att if need be 
    if( ($parser_version > 2.27) && ($t->{twig_doctype}->{internal}=~ m{(^|>)\s*$}) ) 
      { my $text= $XML::Twig::Elt::keep_encoding ? $p->original_string : $p->recognized_string; 
        unless( $text)
          { # this version of XML::Parser does not return the text in the *_string method
            # we need to rebuild it
            my $att_decl="$att $type";
            $att_decl .= " #FIXED"   if( $fixed);
            $att_decl .= " $default" if( defined $default);
            # 2 cases: there is already an attlist on that element or not
            if( $t->{twig_dtd}->{att}->{$gi})
              { # there is already an attlist, add to it
                $t->{twig_doctype}->{internal}=~ s{(<!ATTLIST\s*$gi )(.*?)\n?>}
                                                  { "$1$2\n" . ' ' x length( $1) . "$att_decl\n>"}es;
              }
            else
              { # create the attlist
                 $t->{twig_doctype}->{internal}.= "<!ATTLIST $gi $att_decl>"
              }
          }
      }
    $t->{twig_dtd}->{att}->{$gi}->{$att}= {} ;
    $t->{twig_dtd}->{att}->{$gi}->{$att}->{type}= $type; 
    $t->{twig_dtd}->{att}->{$gi}->{$att}->{default}= $default if( defined $default);
    $t->{twig_dtd}->{att}->{$gi}->{$att}->{fixed}= $fixed; 
    return;
  }

sub _twig_default
   { # warn " in _twig_default...\n"; # DEBUG handler
    
    my( $p, $string)= @_;
    
    my $t= $p->{twig};
   
    # we need to process the data in 2 cases: entity, or spaces after the closing tag

    # after the closing tag (no twig_current and root has been created)
    if(  ! $t->{twig_current} && $t->{twig_root} && $string=~ m{^\s+$}m) { $t->{twig_stored_spaces} .= $string; }

    # process only if we have an entity
    if( $string=~ m{^&([^;]*);$})
      { # the entity has to be pure pcdata, or we have a problem
        if( ($p->original_string=~ m{^<}) && ($p->original_string=~ m{>$}) ) 
          { # string is a tag, entity is in an attribute
            $t->{twig_entities_in_attribute}=1 if( $t->{twig_do_not_escape_amp_in_atts});
          }
        else
          { my $ent;
            if( $t->{twig_keep_encoding}) 
              { _twig_char( $p, $string); 
                $ent= substr( $string, 1, -1);
              }
            else
              { $ent= _twig_insert_ent( $t, $string); 
              }

            return $ent;
          }
      }
  }
    
sub _twig_insert_ent
  { 
    my( $t, $string)=@_;

    my $twig_current= $t->{twig_current};

    my $ent=  $t->{twig_elt_class}->new( $ENT);
    $ent->{ent}=  $string;

    _add_or_discard_stored_spaces( $t);
    
    if( $t->{twig_in_pcdata})
      { # create the node as a sibling of the #PCDATA

        $ent->{prev_sibling}=$twig_current; if( $XML::Twig::weakrefs) { weaken( $ent->{prev_sibling});} ;
        $twig_current->{next_sibling}=  $ent;
        my $parent= $twig_current->{parent};
        $ent->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $ent->{parent});} ;
         $parent->{empty}=0; $parent->{last_child}=$ent; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ;
        # the twig_current is now the parent
        delete $twig_current->{'twig_current'};
        $t->{twig_current}= $parent;
        # we left pcdata
        $t->{twig_in_pcdata}=0;
      }
    else
      { # create the node as a child of the current element
        $ent->{parent}=$twig_current; if( $XML::Twig::weakrefs) { weaken( $ent->{parent});} ;
        if( my $prev_sibling= $twig_current->{last_child})
          { $ent->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $ent->{prev_sibling});} ;
            $prev_sibling->{next_sibling}=  $ent;
          }
        else
          { if( $twig_current) { $twig_current->{first_child}=  $ent; } }
        if( $twig_current) {  $twig_current->{empty}=0; $twig_current->{last_child}=$ent; if( $XML::Twig::weakrefs) { weaken( $twig_current->{last_child});} ; }
      }

    # meant to trigger entity handler, does not seem to be activated at this time
    #if( my $handler= $t->{twig_handlers}->{gi}->{$ENT})
    #  { local $_= $ent; $handler->( $t, $ent); }

    return $ent;
  }

sub parser
  { return $_[0]->{twig_parser}; }

# returns the declaration text (or a default one)
sub xmldecl
  { my $t= shift;
    return '' unless( $t->{twig_xmldecl} || $t->{output_encoding});
    my $decl_string;
    my $decl= $t->{twig_xmldecl};
    if( $decl)
      { my $version= $decl->{version};
        $decl_string= q{<?xml};
        $decl_string .= qq{ version="$version"};

        # encoding can either have been set (in $decl->{output_encoding})
        # or come from the document (in $decl->{encoding})
        if( $t->{output_encoding})
          { my $encoding= $t->{output_encoding};
            $decl_string .= qq{ encoding="$encoding"};
          }
        elsif( $decl->{encoding})
          { my $encoding= $decl->{encoding};
            $decl_string .= qq{ encoding="$encoding"};
          }
    
        if( defined( $decl->{standalone}))
          { $decl_string .= q{ standalone="};  
            $decl_string .= $decl->{standalone} ? "yes" : "no";  
            $decl_string .= q{"}; 
          }
      
        $decl_string .= "?>\n";
      }
    else
      { my $encoding= $t->{output_encoding};
        $decl_string= qq{<?xml version="1.0" encoding="$encoding"?>};
      }
      
    my $output_filter= XML::Twig::Elt::output_filter();
    return $output_filter ? $output_filter->( $decl_string) : $decl_string;
  }

sub set_doctype
  { my( $t, $name, $system, $public, $internal)= @_;
    $t->{twig_doctype}= {} unless defined $t->{twig_doctype};
    my $doctype= $t->{twig_doctype};
    $doctype->{name}     = $name     if( defined $name);
    $doctype->{sysid}    = $system   if( defined $system);
    $doctype->{pub}      = $public   if( defined $public);
    $doctype->{internal} = $internal if( defined $internal);
  }

sub doctype_name
  { my $t= shift;
    my $doctype= $t->{twig_doctype} or return '';
    return $doctype->{name} || '';
  }

sub system_id
  { my $t= shift;
    my $doctype= $t->{twig_doctype} or return '';
    return $doctype->{sysid} || '';
  }

sub public_id
  { my $t= shift;
    my $doctype= $t->{twig_doctype} or return '';
    return $doctype->{pub} || '';
  }

sub internal_subset
  { my $t= shift;
    my $doctype= $t->{twig_doctype} or return '';
    return $doctype->{internal} || '';
  }

# return the dtd object
sub dtd
  { my $t= shift;
    return $t->{twig_dtd};
  }

# return an element model, or the list of element models
sub model
  { my $t= shift;
    my $elt= shift;
    return $t->dtd->{model}->{$elt} if( $elt);
    return (sort keys %{$t->dtd->{model}});
  }

        
# return the entity_list object 
sub entity_list
  { my $t= shift;
    return $t->{twig_entity_list};
  }

# return the list of entity names 
sub entity_names
  { my $t= shift;
    return $t->entity_list->entity_names;
  }

# return the entity object 
sub entity
  { my $t= shift;
    my $entity_name= shift;
    return $t->entity_list->ent( $entity_name);
  }


sub print_prolog
  { my $t= shift;
    my $fh=  isa( $_[0], 'GLOB') || isa( $_[0], 'IO::Scalar')  ? shift : $t->{twig_output_fh} || select() || \*STDOUT;
    ## no critic (TestingAndDebugging::ProhibitNoStrict);
    no strict 'refs';
    print {$fh} $t->prolog( @_);
  }

sub prolog
  { my $t= shift;
    if( $t->{no_prolog}){ return ''; }

    return   $t->{no_prolog}             ? '' 
           : defined $t->{no_dtd_output} ? $t->xmldecl
           :                               $t->xmldecl . $t->doctype( @_);
  }

sub doctype
  { my $t= shift;
    my %args= _normalize_args( @_);
    my $update_dtd = $args{UpdateDTD} || '';
    my $doctype_text='';
    
    my $doctype= $t->{twig_doctype};

    if( $doctype)
      { $doctype_text .= qq{<!DOCTYPE $doctype->{name}} if( $doctype->{name});
        $doctype_text .= qq{ PUBLIC "$doctype->{pub}"}  if( $doctype->{pub});
        $doctype_text .= qq{ SYSTEM}                    if( $doctype->{sysid} && !$doctype->{pub});
        $doctype_text .= qq{ "$doctype->{sysid}"}       if( $doctype->{sysid});
      }

    if( $update_dtd)
      { if( $doctype)  
          { my $internal=$doctype->{internal};
            # awfull hack, but at least it works a little better that what was there before
            if( $internal)
              { # remove entity declarations (they will be re-generated from the updated entity list)
                $internal=~ s{<! \s* ENTITY \s+ $REG_NAME \s+ ( ("[^"]*"|'[^']*') \s* | SYSTEM [^>]*) >\s*}{}xg;
                $internal=~ s{^\n}{};
              }
            $internal .= $t->entity_list->text ||'' if( $t->entity_list);
            if( $internal) { $doctype_text .= "[\n$internal]>\n"; }
          }
        elsif( !$t->{'twig_dtd'} && keys %{$t->entity_list}) 
          { $doctype_text .= "<!DOCTYPE " . $t->root->gi . " [\n" . $t->entity_list->text . "\n]>";;}
        else
          { $doctype_text= $t->{twig_dtd};
            $doctype_text .= $t->dtd_text;
          }            
      }
    elsif( $doctype)
      { if( my $internal= $doctype->{internal}) 
          { # add opening and closing brackets if not already there
            # plus some spaces and newlines for a nice formating
            # I test it here because I can't remember which version of
            # XML::Parser need it or not, nor guess which one will in the
            # future, so this about the best I can do
            $internal=~ s{^\s*(\[\s*)?}{ [\n};
            $internal=~ s{\s*(\]\s*(>\s*)?)?\s*$}{\n]>\n};
            $doctype_text .=  $internal; 
          }
      }
      
    if( $doctype_text)
      {
        # terrible hack, as I can't figure out in which case the darn prolog
        # should get an extra > (depends on XML::Parser and expat versions)
        $doctype_text=~ s/(>\s*)*$/>\n/; # if($doctype_text);

        my $output_filter= XML::Twig::Elt::output_filter();
        return $output_filter ? $output_filter->( $doctype_text) : $doctype_text;
      }
    else
      { return $doctype_text; }
  }

sub _leading_cpi
  { my $t= shift;
    my $leading_cpi= $t->{leading_cpi} || return '';
    return $leading_cpi->sprint( 1);
  }

sub _trailing_cpi
  { my $t= shift;
    my $trailing_cpi= $t->{trailing_cpi} || return '';
    return $trailing_cpi->sprint( 1);
  }

sub _trailing_cpi_text
  { my $t= shift;
    return $t->{trailing_cpi_text} || '';
  }

sub print_to_file
  { my( $t, $filename)= (shift, shift);
    my $out_fh;
    my $mode= $t->{twig_keep_encoding} ? '>' : '>:utf8';                             # >= perl 5.8
    open( $out_fh, $mode, $filename) or _croak( "cannot create file $filename: $!"); # >= perl 5.8
    $t->print( $out_fh, @_);
    close $out_fh;
    return $t;
  }

sub print
  { my $t= shift;
    my $fh=  isa( $_[0], 'GLOB') || isa( $_[0], 'IO::Scalar')  ? shift : undef;
    my %args= _normalize_args( @_);

    my $old_select    = defined $fh                  ? select $fh                                 : undef;
    my $old_pretty    = defined ($args{PrettyPrint}) ? $t->set_pretty_print( $args{PrettyPrint})  : undef;
    my $old_empty_tag = defined ($args{EmptyTags})   ? $t->set_empty_tag_style( $args{EmptyTags}) : undef;

    #if( !$t->{encoding} || lc( $t->{encoding}) eq 'utf-8') { my $out= $fh || \*STDOUT; binmode $out, ':utf8'; }

    if( $perl_version > 5.006 && ! $t->{twig_keep_encoding}) 
      { if( grep /useperlio=define/, `$^X -V`) # we can only use binmode :utf8 if perl was compiled with useperlio
          { binmode( $fh || \*STDOUT, ":utf8" ); }
      }

     print  $t->prolog( %args) . $t->_leading_cpi( %args);
     $t->{twig_root}->print;
     print $t->_trailing_cpi        # trailing comments and pi's (elements, in 'process' mode)
         . $t->_trailing_cpi_text   # trailing comments and pi's (in 'keep' mode)
         . ( ($t->{twig_keep_spaces}||'') && ($t->{trailing_spaces} || ''))
         ;

    
    $t->set_pretty_print( $old_pretty)       if( defined $old_pretty); 
    $t->set_empty_tag_style( $old_empty_tag) if( defined $old_empty_tag); 
    if( $fh) { select $old_select; }

    return $t;
  }


sub flush
  { my $t= shift;

    $t->_trigger_tdh if $t->{twig_tdh};

    return if( $t->{twig_completely_flushed});
  
    my $fh=  isa( $_[0], 'GLOB') || isa( $_[0], 'IO::Scalar') ? shift : undef;
    my $old_select= defined $fh ? select $fh : undef;
    my $up_to= ref $_[0] ? shift : undef;
    my %args= _normalize_args( @_);

    my $old_pretty;
    if( defined $args{PrettyPrint})
      { $old_pretty= $t->set_pretty_print( $args{PrettyPrint}); 
        delete $args{PrettyPrint};
      }

     my $old_empty_tag_style;
     if( $args{EmptyTags})
      { $old_empty_tag_style= $t->set_empty_tag_style( $args{EmptyTags}); 
        delete $args{EmptyTags};
      }


    # the "real" last element processed, as _twig_end has closed it
    my $last_elt;
    my $flush_trailing_data=0;
    if( $up_to)
      { $last_elt= $up_to; }
    elsif( $t->{twig_current})
      { $last_elt= $t->{twig_current}->_last_child; }
    else
      { $last_elt= $t->{twig_root};
        $flush_trailing_data=1;
        $t->{twig_completely_flushed}=1;
      }

    # flush the DTD unless it has ready flushed (ie root has been flushed)
    my $elt= $t->{twig_root};
    unless( $elt->_flushed)
      { # store flush info so we can auto-flush later
        if( $t->{twig_autoflush})
          { $t->{twig_autoflush_data}={};
            $t->{twig_autoflush_data}->{fh}   = $fh  if( $fh);
            $t->{twig_autoflush_data}->{args} = \@_  if( @_);
          }
        $t->print_prolog( %args); 
        print $t->_leading_cpi;
      }

    while( $elt)
      { my $next_elt; 
        if( $last_elt && $last_elt->in( $elt))
          { 
            unless( $elt->_flushed) 
              { # just output the front tag
                print $elt->start_tag();
                $elt->_set_flushed;
              }
            $next_elt= $elt->{first_child};
          }
        else
          { # an element before the last one or the last one,
            $next_elt= $elt->{next_sibling};  
            $elt->_flush();
            $elt->delete; 
            last if( $last_elt && ($elt == $last_elt));
          }
        $elt= $next_elt;
      }

    if( $flush_trailing_data)
      { print $t->_trailing_cpi        # trailing comments and pi's (elements, in 'process' mode)
            , $t->_trailing_cpi_text   # trailing comments and pi's (in 'keep' mode)
      }

    select $old_select if( defined $old_select);
    $t->set_pretty_print( $old_pretty) if( defined $old_pretty); 
    $t->set_empty_tag_style( $old_empty_tag_style) if( defined $old_empty_tag_style); 

    if( my $ids= $t->{twig_id_list}) 
      { while( my ($id, $elt)= each %$ids) 
          { if( ! defined $elt) 
             { delete $t->{twig_id_list}->{$id} } 
          }
      }

    return $t;
  }


# flushes up to an element
# this method just reorders the arguments and calls flush
sub flush_up_to
  { my $t= shift;
    my $up_to= shift;
    if( isa( $_[0], 'GLOB') || isa( $_[0], 'IO::Scalar'))
      { my $fh=  shift;
        $t->flush( $fh, $up_to, @_);
      }
    else
      { $t->flush( $up_to, @_); }

    return $t;
  }

    
# same as print except the entire document text is returned as a string
sub sprint
  { my $t= shift;
    my %args= _normalize_args( @_);

    my $old_pretty;
    if( defined $args{PrettyPrint})
      { $old_pretty= $t->set_pretty_print( $args{PrettyPrint}); 
        delete $args{PrettyPrint};
      }

     my $old_empty_tag_style;
     if( defined $args{EmptyTags})
      { $old_empty_tag_style= $t->set_empty_tag_style( $args{EmptyTags}); 
        delete $args{EmptyTags};
      }
      
    my $string=   $t->prolog( %args)       # xml declaration and doctype
                . $t->_leading_cpi( %args) # leading comments and pi's in 'process' mode
                . $t->{twig_root}->sprint  
                . $t->_trailing_cpi        # trailing comments and pi's (elements, in 'process' mode)
                . $t->_trailing_cpi_text   # trailing comments and pi's (in 'keep' mode)
                ;
    if( $t->{twig_keep_spaces} && $t->{trailing_spaces}) { $string .= $t->{trailing_spaces}; }

    $t->set_pretty_print( $old_pretty) if( defined $old_pretty); 
    $t->set_empty_tag_style( $old_empty_tag_style) if( defined $old_empty_tag_style); 

    return $string;
  }
    

# this method discards useless elements in a tree
# it does the same thing as a flush except it does not print it
# the second argument is an element, the last purged element
# (this argument is usually set through the purge_up_to method)
sub purge
  { my $t= shift;
    my $up_to= shift;

    $t->_trigger_tdh if $t->{twig_tdh};

    # the "real" last element processed, as _twig_end has closed it
    my $last_elt;
    if( $up_to)
      { $last_elt= $up_to; }
    elsif( $t->{twig_current})
      { $last_elt= $t->{twig_current}->_last_child; }
    else
      { $last_elt= $t->{twig_root}; }
    
    my $elt= $t->{twig_root};

    while( $elt)
      { my $next_elt; 
        if( $last_elt && $last_elt->in( $elt))
          { $elt->_set_flushed;
            $next_elt= $elt->{first_child};
          }
        else
          { # an element before the last one or the last one,
            $next_elt= $elt->{next_sibling};  
            $elt->delete; 
            last if( $last_elt && ($elt == $last_elt) );
          }
        $elt= $next_elt;
      }

    if( my $ids= $t->{twig_id_list}) 
      { while( my ($id, $elt)= each %$ids) { if( ! defined $elt) { delete $t->{twig_id_list}->{$id} } } }

    return $t;
  }
    
# flushes up to an element. This method just calls purge
sub purge_up_to
  { my $t= shift;
    return $t->purge( @_);
  }

sub root
  { return $_[0]->{twig_root}; }

sub normalize
  { return $_[0]->root->normalize; }


# create accessor methods on attribute names
{ my %accessor; # memorize accessor names so re-creating them won't trigger an error
sub att_accessors
  { 
    my $twig_or_class= shift;
    my $elt_class= ref $twig_or_class ? $twig_or_class->{twig_elt_class}
                                      : 'XML::Twig::Elt'
                                      ;
    ## no critic (TestingAndDebugging::ProhibitNoStrict);
    no strict 'refs';
    foreach my $att (@_)
      { _croak( "attempt to redefine existing method $att using att_accessors")
          if( $elt_class->can( $att) && !$accessor{$att});

        if( !$accessor{$att})
          { *{"$elt_class\::$att"}=
                sub
                    :lvalue                                  # > perl 5.5
                  { my $elt= shift;
                    if( @_) { $elt->{att}->{$att}= $_[0]; }
                    $elt->{att}->{$att};
                  };
            $accessor{$att}=1;
          }
      }
    return $twig_or_class;
  }
}

{ my %accessor; # memorize accessor names so re-creating them won't trigger an error
sub elt_accessors
  { 
    my $twig_or_class= shift;
    my $elt_class= ref $twig_or_class ? $twig_or_class->{twig_elt_class}
                                      : 'XML::Twig::Elt'
                                      ;

    # if arg is a hash ref, it's exp => name, otherwise it's a list of tags
    my %exp_to_alias= ref( $_[0]) && isa( $_[0], 'HASH') ? %{$_[0]}
                                                         : map { $_ => $_ } @_;
    ## no critic (TestingAndDebugging::ProhibitNoStrict);
    no strict 'refs';
    while( my( $alias, $exp)= each %exp_to_alias )
      { if( $elt_class->can( $alias) && !$accessor{$alias})
          { _croak( "attempt to redefine existing method $alias using elt_accessors"); }

        if( !$accessor{$alias})
          { *{"$elt_class\::$alias"}= 
                sub
                  { my $elt= shift;
                    return wantarray ? $elt->children( $exp) : $elt->first_child( $exp);
                  };
            $accessor{$alias}=1;
          }                                            
      }
    return $twig_or_class;
  }
}

{ my %accessor; # memorize accessor names so re-creating them won't trigger an error
sub field_accessors
  { 
    my $twig_or_class= shift;
    my $elt_class= ref $twig_or_class ? $twig_or_class->{twig_elt_class}
                                      : 'XML::Twig::Elt'
                                      ;
    ## no critic (TestingAndDebugging::ProhibitNoStrict);
    no strict 'refs';
    foreach my $exp (@_)
      { _croak( "attempt to redefine existing method $exp using field_accessors")
          if( $elt_class->can( $exp) && !$accessor{$exp});

        if( !$accessor{$exp})                                
          { *{"$elt_class\::$exp"}=                          
                sub                                          
                  { my $elt= shift;                          
                    $elt->field( $exp)                       
                  };                                         
            $accessor{$exp}=1;                               
          }                                                  
      }
    return $twig_or_class;
  }
}



sub first_elt
  { my( $t, $cond)= @_;
    my $root= $t->root || return undef;
    return $root if( $root->passes( $cond));
    return $root->next_elt( $cond); 
  }

sub last_elt
  { my( $t, $cond)= @_;
    my $root= $t->root || return undef;
    return $root->last_descendant( $cond); 
  }

sub next_n_elt
  { my( $t, $offset, $cond)= @_;
    $offset -- if( $t->root->matches( $cond) );
    return $t->root->next_n_elt( $offset, $cond);
  }

sub get_xpath
  { my $twig= shift;
    if( isa( $_[0], 'ARRAY'))
      { my $elt_array= shift;
        return _unique_elts( map { $_->get_xpath( @_) } @$elt_array);
      }
    else
      { return $twig->root->get_xpath( @_); }
  }

# get a list of elts and return a sorted list of unique elts
sub _unique_elts
  { my @sorted= sort { $a ->cmp( $b) } @_;
    my @unique;
    while( my $current= shift @sorted)
      { push @unique, $current unless( @unique && ($unique[-1] == $current)); }
    return @unique;
  }

sub findvalue
  { my $twig= shift;
    if( isa( $_[0], 'ARRAY'))
      { my $elt_array= shift;
        return join( '', map { $_->findvalue( @_) } @$elt_array);
      }
    else
      { return $twig->root->findvalue( @_); }
  }

sub findvalues
  { my $twig= shift;
    if( isa( $_[0], 'ARRAY'))
      { my $elt_array= shift;
        return map { $_->findvalues( @_) } @$elt_array;
      }
    else
      { return $twig->root->findvalues( @_); }
  }

sub set_id_seed
  { my $t= shift;
    XML::Twig::Elt->set_id_seed( @_);
    return $t;
  }

# return an array ref to an index, or undef
sub index
  { my( $twig, $name, $index)= @_;
    return defined( $index) ? $twig->{_twig_index}->{$name}->[$index] : $twig->{_twig_index}->{$name};
  }

# return a list with just the root
# if a condition is given then return an empty list unless the root matches
sub children
  { my( $t, $cond)= @_;
    my $root= $t->root;
    unless( $cond && !($root->passes( $cond)) )
      { return ($root); }
    else
      { return (); }
  }
  
sub _children { return ($_[0]->root); }

# weird, but here for completude
# used to solve (non-sensical) /doc[1] XPath queries
sub child
  { my $t= shift;
    my $nb= shift;
    return ($t->children( @_))[$nb];
  }

sub descendants
  { my( $t, $cond)= @_;
    my $root= $t->root;
    if( $root->passes( $cond) )
      { return ($root, $root->descendants( $cond)); }
    else
      { return ( $root->descendants( $cond)); }
  }

sub simplify  { my $t= shift; $t->root->simplify( @_);  }
sub subs_text { my $t= shift; $t->root->subs_text( @_); }
sub trim      { my $t= shift; $t->root->trim( @_);      }


sub set_keep_encoding
  { my( $t, $keep)= @_;
    $t->{twig_keep_encoding}= $keep;
    $t->{NoExpand}= $keep;
    return XML::Twig::Elt::set_keep_encoding( $keep);
   }

sub set_expand_external_entities
  { return XML::Twig::Elt::set_expand_external_entities( @_); }

sub escape_gt
  { my $t= shift; $t->{twig_escape_gt}= 1; return XML::Twig::Elt::escape_gt( @_); }

sub do_not_escape_gt
  { my $t= shift; $t->{twig_escape_gt}= 0; return XML::Twig::Elt::do_not_escape_gt( @_); }

# WARNING: at the moment the id list is not updated reliably
sub elt_id
  { return $_[0]->{twig_id_list}->{$_[1]}; }

# change it in ALL twigs at the moment
sub change_gi 
  { my( $twig, $old_gi, $new_gi)= @_;
    my $index;
    return unless($index= $XML::Twig::gi2index{$old_gi});
    $XML::Twig::index2gi[$index]= $new_gi;
    delete $XML::Twig::gi2index{$old_gi};
    $XML::Twig::gi2index{$new_gi}= $index;
    return $twig;
  }


# builds the DTD from the stored (possibly updated) data
sub dtd_text
  { my $t= shift;
    my $dtd= $t->{twig_dtd};
    my $doctype= $t->{twig_doctype} or return '';
    my $string= "<!DOCTYPE ".$doctype->{name};

    $string .= " [\n";

    foreach my $gi (@{$dtd->{elt_list}})
      { $string.= "<!ELEMENT $gi ".$dtd->{model}->{$gi}.">\n" ;
        if( $dtd->{att}->{$gi})
          { my $attlist= $dtd->{att}->{$gi};
            $string.= "<!ATTLIST $gi\n";
            foreach my $att ( sort keys %{$attlist})
              { 
                if( $attlist->{$att}->{fixed})
                  { $string.= "   $att $attlist->{$att}->{type} #FIXED $attlist->{$att}->{default}"; }
                else
                  { $string.= "   $att $attlist->{$att}->{type} $attlist->{$att}->{default}"; }
                $string.= "\n";
              }
            $string.= ">\n";
          }
      }
    $string.= $t->entity_list->text if( $t->entity_list);
    $string.= "\n]>\n";
    return $string;
  }
        
# prints the DTD from the stored (possibly updated) data
sub dtd_print
  { my $t= shift;
    my $fh=  isa( $_[0], 'GLOB') || isa( $_[0], 'IO::Scalar')  ? shift : undef;
    if( $fh) { print $fh $t->dtd_text; }
    else     { print $t->dtd_text;     }
    return $t;
  }

# build the subs that call directly expat
BEGIN
  { my @expat_methods= qw( depth in_element within_element context
                           current_line current_column current_byte
                           recognized_string original_string 
                           xpcroak xpcarp 
                           xml_escape
                           base current_element element_index 
                           position_in_context);
    foreach my $method (@expat_methods)
      { 
        ## no critic (TestingAndDebugging::ProhibitNoStrict);
        no strict 'refs';
        *{$method}= sub { my $t= shift;
                          _croak( "calling $method after parsing is finished") unless( $t->{twig_parsing}); 
                          return $t->{twig_parser}->$method(@_); 
                        };
      }
  }

sub path
  { my( $t, $gi)= @_;
    if( $t->{twig_map_xmlns})
      { return "/" . join( "/", map { $t->_replace_prefix( $_)} ($t->{twig_parser}->context, $gi)); }
    else
      { return "/" . join( "/", ($t->{twig_parser}->context, $gi)); }
  }

sub finish
  { my $t= shift;
    return $t->{twig_parser}->finish;
  }

# just finish the parse by printing the appdynamics of the document
sub finish_print
  { my( $t, $fh)= @_;
    my $old_fh;
    unless( defined $fh)
      { $t->_set_fh_to_twig_output_fh(); }
    elsif( defined $fh)
      { $old_fh= select $fh; 
        $t->{twig_original_selected_fh}= $old_fh if( $old_fh); 
      }
    
    my $p=$t->{twig_parser};
    if( $t->{twig_keep_encoding})
      { $p->setHandlers( %twig_handlers_finish_print); }
    else
      { $p->setHandlers( %twig_handlers_finish_print_original); }
    return $t;
  }

sub set_remove_cdata { return XML::Twig::Elt::set_remove_cdata( @_); }

sub output_filter          { return XML::Twig::Elt::output_filter( @_);          }
sub set_output_filter      { return XML::Twig::Elt::set_output_filter( @_);      }

sub output_text_filter     { return XML::Twig::Elt::output_text_filter( @_);     }
sub set_output_text_filter { return XML::Twig::Elt::set_output_text_filter( @_); }

sub set_input_filter
  { my( $t, $input_filter)= @_;
    my $old_filter= $t->{twig_input_filter};
      if( !$input_filter || isa( $input_filter, 'CODE') )
        { $t->{twig_input_filter}= $input_filter; }
      elsif( $input_filter eq 'latin1')
        {  $t->{twig_input_filter}= latin1(); }
      elsif( $filter{$input_filter})
        {  $t->{twig_input_filter}= $filter{$input_filter}; }
      else
        { _croak( "invalid input filter: $input_filter"); }
      
      return $old_filter;
    }

sub set_empty_tag_style
  { return XML::Twig::Elt::set_empty_tag_style( @_); }

sub set_pretty_print
  { return XML::Twig::Elt::set_pretty_print( @_); }

sub set_quote
  { return XML::Twig::Elt::set_quote( @_); }

sub set_indent
  { return XML::Twig::Elt::set_indent( @_); }

sub set_keep_atts_order
  { shift; return XML::Twig::Elt::set_keep_atts_order( @_); }

sub keep_atts_order
  { return XML::Twig::Elt::keep_atts_order( @_); }

sub set_do_not_escape_amp_in_atts
  { return XML::Twig::Elt::set_do_not_escape_amp_in_atts( @_); }

# save and appdynamicsore package globals (the ones in XML::Twig::Elt)
# should probably return the XML::Twig object itself, but instead
# returns the state (as a hashref) for backward compatibility
sub save_global_state
  { my $t= shift;
    return $t->{twig_saved_state}= XML::Twig::Elt::global_state();
  }

sub appdynamicsore_global_state
  { my $t= shift;
    XML::Twig::Elt::set_global_state( $t->{twig_saved_state});
  }

sub global_state
  { return XML::Twig::Elt::global_state(); }

sub set_global_state
  {  return XML::Twig::Elt::set_global_state( $_[1]); }

sub dispose
  { my $t= shift;
    $t->DESTROY;
    return;
  }
  
sub DESTROY
  { my $t= shift;
    if( $t->{twig_root} && isa(  $t->{twig_root}, 'XML::Twig::Elt')) 
      { $t->{twig_root}->delete } 

    # added to break circular references
    undef $t->{twig};
    undef $t->{twig_root}->{twig} if( $t->{twig_root});
    undef $t->{twig_parser};
    
    undef %$t;# prevents memory leaks (especially when using mod_perl)
    undef $t;
  }        


#
#  non standard handlers
#

# kludge: expat 1.95.2 calls both Default AND Doctype handlers
# so if the default handler finds '<!DOCTYPE' then it must 
# unset itself (_twig_print_doctype will reset it)
sub _twig_print_check_doctype
   { # warn " in _twig_print_check_doctype...\n"; # DEBUG handler
    
    my $p= shift;
    my $string= $p->recognized_string();
    if( $string eq '<!DOCTYPE') 
      { 
        $p->setHandlers( Default => undef); 
        $p->setHandlers( Entity => undef); 
        $expat_1_95_2=1; 
      }
    else                        
      { print $string; }

    return;
  }


sub _twig_print
   { # warn " in _twig_print...\n"; # DEBUG handler
    my $p= shift;
    if( $expat_1_95_2 && ($p->recognized_string eq '[') && !$p->{twig}->{expat_1_95_2_seen_bracket})
      { # otherwise the opening square bracket of the doctype gets printed twice 
        $p->{twig}->{expat_1_95_2_seen_bracket}=1;
      }
    else
      { if( $p->{twig}->{twig_right_after_root})
          { my $s= $p->recognized_string(); print $s if $s=~ m{\S}; }
        else
          { print $p->recognized_string(); }
      }
    return;
  }
# recognized_string does not seem to work for entities, go figure!
# so this handler is used to print them anyway
sub _twig_print_entity
   { # warn " in _twig_print_entity...\n"; # DEBUG handler
    my $p= shift; 
    XML::Twig::Entity->new( @_)->print;
  }

# kludge: expat 1.95.2 calls both Default AND Doctype handlers
# so if the default handler finds '<!DOCTYPE' then it must 
# unset itself (_twig_print_doctype will reset it)
sub _twig_print_original_check_doctype
   { # warn " in _twig_print_original_check_doctype...\n"; # DEBUG handler
    
    my $p= shift;
    my $string= $p->original_string();
    if( $string eq '<!DOCTYPE') 
      { $p->setHandlers( Default => undef); 
        $p->setHandlers( Entity => undef); 
        $expat_1_95_2=1; 
      }
    else                        
      { print $string; }

    return;    
  }

sub _twig_print_original
   { # warn " in _twig_print_original...\n"; # DEBUG handler
    my $p= shift; 
    print $p->original_string();
    return;    
  }


sub _twig_print_original_doctype
   { # warn " in _twig_print_original_doctype...\n"; # DEBUG handler
    
    my(  $p, $name, $sysid, $pubid, $internal)= @_;
    if( $name)
      { # with recent versions of XML::Parser original_string does not work,
        # hence we need to rebuild the doctype declaration
        my $doctype='';
        $doctype .= qq{<!DOCTYPE $name}    if( $name);
        $doctype .=  qq{ PUBLIC  "$pubid"}  if( $pubid);
        $doctype .=  qq{ SYSTEM}            if( $sysid && !$pubid);
        $doctype .=  qq{ "$sysid"}          if( $sysid); 
        $doctype .=  ' [' if( $internal && !$expat_1_95_2) ;
        $doctype .=  qq{>} unless( $internal || $expat_1_95_2);
        $p->{twig}->{twig_doctype}->{has_internal}=$internal;
        print $doctype;
      }
    $p->setHandlers( Default => \&_twig_print_original);
    return;    
  }

sub _twig_print_doctype
   { # warn " in _twig_print_doctype...\n"; # DEBUG handler
    my(  $p, $name, $sysid, $pubid, $internal)= @_;
    if( $name)
      { # with recent versions of XML::Parser original_string does not work,
        # hence we need to rebuild the doctype declaration
        my $doctype='';
        $doctype .= qq{<!DOCTYPE $name}    if( $name);
        $doctype .=  qq{ PUBLIC  "$pubid"}  if( $pubid);
        $doctype .=  qq{ SYSTEM}            if( $sysid && !$pubid);
        $doctype .=  qq{ "$sysid"}          if( $sysid); 
        $doctype .=  ' [' if( $internal) ;
        $doctype .=  qq{>} unless( $internal || $expat_1_95_2);
        $p->{twig}->{twig_doctype}->{has_internal}=$internal;
        print $doctype;
      }
    $p->setHandlers( Default => \&_twig_print);
    return;    
  }


sub _twig_print_original_default
   { # warn " in _twig_print_original_default...\n"; # DEBUG handler
    my $p= shift;
    print $p->original_string();
    return;    
  }

# account for the case where the element is empty
sub _twig_print_end_original
   { # warn " in _twig_print_end_original...\n"; # DEBUG handler
    my $p= shift;
    print $p->original_string();
    return;    
  }

sub _twig_start_check_roots
   { # warn " in _twig_start_check_roots...\n"; # DEBUG handler
    my $p= shift;
    my $gi= shift;
    
    my $t= $p->{twig};
    
    my $fh= $t->{twig_output_fh} || select() || \*STDOUT;

    unless( $p->depth == 0)
      { if( $t->{twig_map_xmlns}) { _replace_ns( $t, \$gi, \@_); }
      }

    push @{$t->{_twig_context_stack}}, { _tag => $gi, @_};
    my %att= @_;

    if( _handler( $t, $t->{twig_roots}, $gi))
      { $p->setHandlers( %twig_handlers); # appdynamicsore regular handlers
        $t->{twig_root_depth}= $p->depth; 
        pop @{$t->{_twig_context_stack}}; # will be pushed back in _twig_start
        _twig_start( $p, $gi, @_);
        return;
      }

    # $tag will always be true if it needs to be printed (the tag string is never empty)
    my $tag= $t->{twig_default_print} ? $t->{twig_keep_encoding} ? $p->original_string
                                                                 : $p->recognized_string
                                      : '';

    if( $p->depth == 0)
      { 
        ## no critic (TestingAndDebugging::ProhibitNoStrict);
        no strict 'refs';
        print {$fh} $tag if( $tag);
        pop @{$t->{_twig_context_stack}}; # will be pushed back in _twig_start
        _twig_start( $p, $gi, @_);
        $t->root->_set_flushed; # or the root start tag gets output the first time we flush
      }
    elsif( $t->{twig_starttag_handlers})
      { # look for start tag handlers

        my @handlers= _handler( $t, $t->{twig_starttag_handlers}, $gi);
        my $last_handler_res;
        foreach my $handler ( @handlers)
          { $last_handler_res= $handler->($t, $gi, %att);
            last unless $last_handler_res;
          }
        ## no critic (TestingAndDebugging::ProhibitNoStrict);
        no strict 'refs';
        print {$fh} $tag if( $tag && (!@handlers || $last_handler_res));   
      }
    else
      { 
        ## no critic (TestingAndDebugging::ProhibitNoStrict);
        no strict 'refs';
        print {$fh} $tag if( $tag); 
      }  
    return;    
  }

sub _twig_end_check_roots
   { # warn " in _twig_end_check_roots...\n"; # DEBUG handler
    
    my( $p, $gi, %att)= @_;
    my $t= $p->{twig};
    # $tag can be empty (<elt/>), hence the undef and the tests for defined
    my $tag= $t->{twig_default_print} ? $t->{twig_keep_encoding} ? $p->original_string
                                                                 : $p->recognized_string
                                      : undef;
    my $fh= $t->{twig_output_fh} || select() || \*STDOUT;
    
    if( $t->{twig_endtag_handlers})
      { # look for end tag handlers
        my @handlers= _handler( $t, $t->{twig_endtag_handlers}, $gi);
        my $last_handler_res=1;
        foreach my $handler ( @handlers)
          { $last_handler_res= $handler->($t, $gi) || last; }
        #if( ! $last_handler_res) 
        #  { pop @{$t->{_twig_context_stack}}; warn "tested";
        #    return;
        #  }
      }
    {
      ## no critic (TestingAndDebugging::ProhibitNoStrict);
      no strict 'refs';
      print {$fh} $tag if( defined $tag);
    }
    if( $p->depth == 0)
      { 
        _twig_end( $p, $gi);  
        $t->root->{end_tag_flushed}=1;
      }

    pop @{$t->{_twig_context_stack}};
    return;    
  }

sub _twig_pi_check_roots
   { # warn " in _twig_pi_check_roots...\n"; # DEBUG handler
    my( $p, $target, $data)= @_;
    my $t= $p->{twig};
    my $pi= $t->{twig_default_print} ? $t->{twig_keep_encoding} ? $p->original_string
                                                                : $p->recognized_string
                                    : undef;
    my $fh= $t->{twig_output_fh} || select() || \*STDOUT;
    
    if( my $handler=    $t->{twig_handlers}->{pi_handlers}->{$target}
                     || $t->{twig_handlers}->{pi_handlers}->{''}
      )
      { # if handler is called on pi, then it needs to be processed as a regular node
        my @flags= qw( twig_process_pi twig_keep_pi);
        my @save= @{$t}{@flags}; # save pi related flags
        @{$t}{@flags}= (1, 0);   # override them, pi needs to be processed
        _twig_pi( @_);           # call handler on the pi
        @{$t}{@flags}= @save;;   # appdynamicsore flag
      }
    else
      { 
        ## no critic (TestingAndDebugging::ProhibitNoStrict);
        no strict 'refs';
        print  {$fh} $pi if( defined( $pi));
      }
    return;    
  }


sub _output_ignored
  { my( $t, $p)= @_;
    my $action= $t->{twig_ignore_action};

    my $get_string= $t->{twig_keep_encoding} ? 'original_string' : 'recognized_string';

    if( $action eq 'print' ) { print $p->$get_string; }
    else
      { my $string_ref;
        if( $action eq 'string') 
          { $string_ref= \$t->{twig_buffered_string}; }
        elsif( ref( $action) && ref( $action) eq 'SCALAR')
          { $string_ref= $action; }
        else
          { _croak( "wrong ignore action: $action"); }

        $$string_ref .= $p->$get_string;
      }
  }
     
        

sub _twig_ignore_start
   { # warn " in _twig_ignore_start...\n"; # DEBUG handler
    
    my( $p, $gi)= @_;
    my $t= $p->{twig};
    $t->{twig_ignore_level}++;
    my $action= $t->{twig_ignore_action}; 

    $t->_output_ignored( $p) unless $action eq 'discard';
    return;    
  }

sub _twig_ignore_end
   { # warn " in _twig_ignore_end...\n"; # DEBUG handler
    
    my( $p, $gi)= @_;
    my $t= $p->{twig};

    my $action= $t->{twig_ignore_action};
    $t->_output_ignored( $p) unless $action eq 'discard';

    $t->{twig_ignore_level}--;

    if( ! $t->{twig_ignore_level})
      { 
        $t->{twig_current}   = $t->{twig_ignore_elt};
        $t->{twig_current}->set_twig_current;

        $t->{twig_ignore_elt}->cut;  # there could possibly be a memory leak here (delete would avoid it,
                                     # but could also delete elements that should not be deleted)

        # appdynamicsore the saved stack to the current level
        splice( @{$t->{_twig_context_stack}}, $p->depth+ 1 );
        #warn "stack: ", _dump_stack( $t->{_twig_context_stack}), "\n";

        $p->setHandlers( @{$t->{twig_saved_handlers}});
        # test for handlers
        if( $t->{twig_endtag_handlers})
          { # look for end tag handlers
            my @handlers= _handler( $t, $t->{twig_endtag_handlers}, $gi);
            my $last_handler_res=1;
            foreach my $handler ( @handlers)
              { $last_handler_res= $handler->($t, $gi) || last; }
          }
        pop @{$t->{_twig_context_stack}};
      };
    return;    
  }

#sub _dump_stack { my( $stack)= @_; return join( ":", map { $_->{_tag} } @$stack); }
    
sub ignore
  { my( $t, $elt, $action)= @_;
    my $current= $t->{twig_current};

    if( ! ($elt && ref( $elt) && isa( $elt, 'XML::Twig::Elt'))) { $elt= $current; }

    #warn "ignore:  current = ", $current->tag, ", elt = ", $elt->tag, ")\n";

    # we need the ($elt == $current->{last_child}) test because the current element is set to the
    # parent _before_ handlers are called (and I can't figure out how to fix this)
    unless( ($elt == $current) || ($current->{last_child} && ($elt == $current->{last_child})) || $current->in( $elt)) 
      { _croak( "element to be ignored must be ancestor of current element"); }

    $t->{twig_ignore_level}= $current == $elt ? 1 : $t->_level_in_stack( $current) - $t->_level_in_stack($elt) + 1;
    #warn "twig_ignore_level:  $t->{twig_ignore_level} (current: ", $current->tag, ", elt: ", $elt->tag, ")\n";
    $t->{twig_ignore_elt}  = $elt;     # save it, so we can delete it later

    $action ||= 'discard'; 
    if( !($action eq 'print' || $action eq 'string' || ( ref( $action) && ref( $action) eq 'SCALAR')))
      { $action= 'discard'; }
   
    $t->{twig_ignore_action}= $action;

    my $p= $t->{twig_parser};
    my @saved_handlers= $p->setHandlers( %twig_handlers_ignore); # set handlers
   
    my $get_string= $t->{twig_keep_encoding} ? 'original_string' : 'recognized_string';

    my $default_handler;

    if( $action ne 'discard')
      { if( $action eq 'print')
          { $p->setHandlers( Default => sub { print $_[0]->$get_string; }); }
        else
          { my $string_ref;
            if( $action eq 'string') 
              { if( ! exists $t->{twig_buffered_string}) { $t->{twig_buffered_string}=''; }
                $string_ref= \$t->{twig_buffered_string}; 
              }
            elsif( ref( $action) && ref( $action) eq 'SCALAR')
              { $string_ref= $action; }
    
            $p->setHandlers( Default =>  sub { $$string_ref .= $_[0]->$get_string; });
          }
        $t->_output_ignored( $p, $action);
      }


    $t->{twig_saved_handlers}= \@saved_handlers;        # save current handlers
  }

sub _level_in_stack
  { my( $t, $elt)= @_;
    my $level=1;
    foreach my $elt_in_stack ( @{$t->{_twig_context_stack}} )
      { if( $elt_in_stack->{_elt} && ($elt == $elt_in_stack->{_elt})) { return $level }
        $level++;
      }
  }



# select $t->{twig_output_fh} and store the current selected fh 
sub _set_fh_to_twig_output_fh
  { my $t= shift;
    my $output_fh= $t->{twig_output_fh};
    if( $output_fh && !$t->{twig_output_fh_selected})
      { # there is an output fh
        $t->{twig_selected_fh}= select(); # store the currently selected fh
        $t->{twig_output_fh_selected}=1;
        select $output_fh;                # select the output fh for the twig
      }
  }

# select the fh that was stored in $t->{twig_selected_fh} 
# (before $t->{twig_output_fh} was selected)
sub _set_fh_to_selected_fh
  { my $t= shift;
    return unless( $t->{twig_output_fh});
    my $selected_fh= $t->{twig_selected_fh};
    $t->{twig_output_fh_selected}=0;
    select $selected_fh;
    return;
  }
  

sub encoding
  { return $_[0]->{twig_xmldecl}->{encoding} if( $_[0]->{twig_xmldecl}); }

sub set_encoding
  { my( $t, $encoding)= @_;
    $t->{twig_xmldecl} ||={};
    $t->set_xml_version( "1.0") unless( $t->xml_version);
    $t->{twig_xmldecl}->{encoding}= $encoding;
    return $t;
  }

sub output_encoding
  { return $_[0]->{output_encoding}; }
  
sub set_output_encoding
  { my( $t, $encoding)= @_;
    my $output_filter= $t->output_filter || '';

    if( ($encoding && $encoding !~ m{^utf-?8$}i) || $t->{twig_keep_encoding} || $output_filter)
      { $t->set_output_filter( _encoding_filter( $encoding || '')); }

    $t->{output_encoding}= $encoding;
    return $t;
  }

sub xml_version
  { return $_[0]->{twig_xmldecl}->{version} if( $_[0]->{twig_xmldecl}); }

sub set_xml_version
  { my( $t, $version)= @_;
    $t->{twig_xmldecl} ||={};
    $t->{twig_xmldecl}->{version}= $version;
    return $t;
  }

sub standalone
  { return $_[0]->{twig_xmldecl}->{standalone} if( $_[0]->{twig_xmldecl}); }

sub set_standalone
  { my( $t, $standalone)= @_;
    $t->{twig_xmldecl} ||={};
    $t->set_xml_version( "1.0") unless( $t->xml_version);
    $t->{twig_xmldecl}->{standalone}= $standalone;
    return $t;
  }


# SAX methods

sub toSAX1
  { _croak( "cannot use toSAX1 while parsing (use flush_toSAX1)") if (defined $_[0]->{twig_parser});
    shift(@_)->_toSAX(@_, \&XML::Twig::Elt::_start_tag_data_SAX1,
                          \&XML::Twig::Elt::_end_tag_data_SAX1
             ); 
  }

sub toSAX2
  { _croak( "cannot use toSAX2 while parsing (use flush_toSAX2)") if (defined $_[0]->{twig_parser});
    shift(@_)->_toSAX(@_, \&XML::Twig::Elt::_start_tag_data_SAX2,
                          \&XML::Twig::Elt::_end_tag_data_SAX2
             ); 
  }


sub _toSAX
  { my( $t, $handler, $start_tag_data, $end_tag_data) = @_;

    if( my $start_document =  $handler->can( 'start_document'))
      { $start_document->( $handler); }
    
    $t->_prolog_toSAX( $handler);
    
    if( $t->root) { $t->root->_toSAX( $handler, $start_tag_data, $end_tag_data) ; }
    if( my $end_document =  $handler->can( 'end_document'))
      { $end_document->( $handler); }
  }


sub flush_toSAX1
  { shift(@_)->_flush_toSAX(@_, \&XML::Twig::Elt::_start_tag_data_SAX1,
                               \&XML::Twig::Elt::_end_tag_data_SAX1
             ); 
  }

sub flush_toSAX2
  { shift(@_)->_flush_toSAX(@_, \&XML::Twig::Elt::_start_tag_data_SAX2,
                               \&XML::Twig::Elt::_end_tag_data_SAX2
             ); 
  }

sub _flush_toSAX
  { my( $t, $handler, $start_tag_data, $end_tag_data)= @_;

    # the "real" last element processed, as _twig_end has closed it
    my $last_elt;
    if( $t->{twig_current})
      { $last_elt= $t->{twig_current}->_last_child; }
    else
      { $last_elt= $t->{twig_root}; }

    my $elt= $t->{twig_root};
    unless( $elt->_flushed)
      { # init unless already done (ie root has been flushed)
        if( my $start_document =  $handler->can( 'start_document'))
          { $start_document->( $handler); }
        # flush the DTD
        $t->_prolog_toSAX( $handler) 
      }

    while( $elt)
      { my $next_elt; 
        if( $last_elt && $last_elt->in( $elt))
          { 
            unless( $elt->_flushed) 
              { # just output the front tag
                if( my $start_element = $handler->can( 'start_element'))
                 { if( my $tag_data= $start_tag_data->( $elt))
                     { $start_element->( $handler, $tag_data); }
                 }
                $elt->_set_flushed;
              }
            $next_elt= $elt->{first_child};
          }
        else
          { # an element before the last one or the last one,
            $next_elt= $elt->{next_sibling};  
            $elt->_toSAX( $handler, $start_tag_data, $end_tag_data);
            $elt->delete; 
            last if( $last_elt && ($elt == $last_elt));
          }
        $elt= $next_elt;
      }
    if( !$t->{twig_parsing}) 
      { if( my $end_document =  $handler->can( 'end_document'))
          { $end_document->( $handler); }
      }
  }


sub _prolog_toSAX
  { my( $t, $handler)= @_;
    $t->_xmldecl_toSAX( $handler);
    $t->_DTD_toSAX( $handler);
  }

sub _xmldecl_toSAX
  { my( $t, $handler)= @_;
    my $decl= $t->{twig_xmldecl};
    my $data= { Version    => $decl->{version},
                Encoding   => $decl->{encoding},
                Standalone => $decl->{standalone},
          };
    if( my $xml_decl= $handler->can( 'xml_decl'))
      { $xml_decl->( $handler, $data); }
  }
                
sub _DTD_toSAX
  { my( $t, $handler)= @_;
    my $doctype= $t->{twig_doctype};
    return unless( $doctype);
    my $data= { Name     => $doctype->{name},
                PublicId => $doctype->{pub},
                SystemId => $doctype->{sysid},
              };

    if( my $start_dtd= $handler->can( 'start_dtd'))
      { $start_dtd->( $handler, $data); }

    # I should call code to export the internal subset here 
    
    if( my $end_dtd= $handler->can( 'end_dtd'))
      { $end_dtd->( $handler); }
  }

# input/output filters

sub latin1 
  { local $SIG{__DIE__};
    if( _use(  'Encode'))
      { return encode_convert( 'ISO-8859-15'); }
    elsif( _use( 'Text::Iconv'))
      { return iconv_convert( 'ISO-8859-15'); }
    elsif( _use( 'Unicode::Map8') && _use( 'Unicode::String'))
      { return unicode_convert( 'ISO-8859-15'); }
    else
      { return \&regexp2latin1; }
  }

sub _encoding_filter
  { 
      { local $SIG{__DIE__};
        my $encoding= $_[1] || $_[0];
        if( _use( 'Encode'))
          { my $sub= encode_convert( $encoding);
            return $sub;
          }
        elsif( _use( 'Text::Iconv'))
          { return iconv_convert( $encoding); }
        elsif( _use( 'Unicode::Map8') && _use( 'Unicode::String'))
          { return unicode_convert( $encoding); }
        }
    _croak( "Encode, Text::Iconv or Unicode::Map8 and Unicode::String need to be installed in order to use encoding options");
  }

# shamelessly lifted from XML::TyePYX (works only with XML::Parse 2.27)
sub regexp2latin1
  { my $text=shift;
    $text=~s{([\xc0-\xc3])(.)}{ my $hi = ord($1);
                                my $lo = ord($2);
                                chr((($hi & 0x03) <<6) | ($lo & 0x3F))
                              }ge;
    return $text;
  }


sub html_encode
  { _use( 'HTML::Entities') or croak "cannot use html_encode: missing HTML::Entities";
    return HTML::Entities::encode_entities($_[0] );
  }

sub safe_encode
  {   my $str= shift;
      if( $perl_version < 5.008)
        { # the no utf8 makes the regexp work in 5.6
          no utf8; # = perl 5.6
          $str =~ s{([\xC0-\xDF].|[\xE0-\xEF]..|[\xF0-\xFF]...)}
                   {_XmlUtf8Decode($1)}egs; 
        }
      else
        { $str= encode( ascii => $str, $FB_HTMLCREF); }
      return $str;
  }

sub safe_encode_hex
  {   my $str= shift;
      if( $perl_version < 5.008)
        { # the no utf8 makes the regexp work in 5.6
          no utf8; # = perl 5.6
          $str =~ s{([\xC0-\xDF].|[\xE0-\xEF]..|[\xF0-\xFF]...)}
                   {_XmlUtf8Decode($1, 1)}egs; 
        }
      else
        { $str= encode( ascii => $str, $FB_XMLCREF); }
      return $str;
  }

# this one shamelessly lifted from XML::DOM
# does NOT work on 5.8.0
sub _XmlUtf8Decode
  { my ($str, $hex) = @_;
    my $len = length ($str);
    my $n;

    if ($len == 2)
      { my @n = unpack "C2", $str;
        $n = (($n[0] & 0x3f) << 6) + ($n[1] & 0x3f);
      }
    elsif ($len == 3)
      { my @n = unpack "C3", $str;
        $n = (($n[0] & 0x1f) << 12) + (($n[1] & 0x3f) << 6) + ($n[2] & 0x3f);
      }
    elsif ($len == 4)
      { my @n = unpack "C4", $str;
        $n = (($n[0] & 0x0f) << 18) + (($n[1] & 0x3f) << 12) 
           + (($n[2] & 0x3f) << 6) + ($n[3] & 0x3f);
      }
    elsif ($len == 1)    # just to be complete...
      { $n = ord ($str); }
    else
      { croak "bad value [$str] for _XmlUtf8Decode"; }

    my $char= $hex ? sprintf ("&#x%x;", $n) : "&#$n;";
    return $char;
  }


sub unicode_convert
  { my $enc= $_[1] ? $_[1] : $_[0]; # so the method can be called on the twig or directly
    _use( 'Unicode::Map8') or croak "Unicode::Map8 not available, needed for encoding filter: $!";
    _use( 'Unicode::String') or croak "Unicode::String not available, needed for encoding filter: $!";
    import Unicode::String qw(utf8);
    my $sub= eval qq{ { $NO_WARNINGS;
                        my \$cnv;
                        BEGIN {  \$cnv= Unicode::Map8->new(\$enc) 
                                     or croak "Can't create converter to \$enc";
                              }
                        sub { return  \$cnv->to8 (utf8(\$_[0])->ucs2); } 
                      } 
                    };
    unless( $sub) { croak $@; }
    return $sub;
  }

sub iconv_convert
  { my $enc= $_[1] ? $_[1] : $_[0]; # so the method can be called on the twig or directly
    _use( 'Text::Iconv') or croak "Text::Iconv not available, needed for encoding filter: $!";
    my $sub= eval qq{ { $NO_WARNINGS;
                        my \$cnv;
                        BEGIN { \$cnv = Text::Iconv->new( 'utf8', \$enc) 
                                     or croak "Can't create iconv converter to \$enc";
                              }
                        sub { return  \$cnv->convert( \$_[0]); } 
                      }       
                    };
    unless( $sub)
      { if( $@=~ m{^Unsupported conversion: Invalid argument})
          { croak "Unsupported encoding: $enc"; }
        else
          { croak $@; }
      }

    return $sub;
  }

sub encode_convert
  { my $enc= $_[1] ? $_[1] : $_[0]; # so the method can be called on the twig or directly
    my $sub=  eval qq{sub { $NO_WARNINGS; return encode( "$enc", \$_[0]); } };
    croak "can't create Encode-based filter: $@" unless( $sub);
    return $sub;
  }


# XML::XPath compatibility
sub getRootNode        { return $_[0]; }
sub getParentNode      { return undef; }
sub getChildNodes      { my @children= ($_[0]->root); return wantarray ? @children : \@children; }

sub _weakrefs     { return $weakrefs;       }
sub _set_weakrefs { $weakrefs=shift() || 0; } # for testing purposes

sub _dump
  { my $t= shift;
    my $dump='';

    $dump="document\n"; # should dump twig level data here
    if( $t->root) { $dump .= $t->root->_dump( @_); }

    return $dump;
    
  }


1;

######################################################################
package XML::Twig::Entity_list;
######################################################################

*isa= *UNIVERSAL::isa;

sub new
  { my $class = shift;
    my $self={ entities => {}, updated => 0};

    bless $self, $class;
    return $self;

  }

sub add_new_ent
  { my $ent_list= shift;
    my $ent= XML::Twig::Entity->new( @_);
    $ent_list->add( $ent);
    return $ent_list;
  }

sub _add_list
  { my( $ent_list, $to_add)= @_;
    my $ents_to_add= $to_add->{entities};
    return $ent_list unless( $ents_to_add && %$ents_to_add);
    @{$ent_list->{entities}}{keys %$ents_to_add}= values %$ents_to_add;
    $ent_list->{updated}=1;
    return $ent_list;
  }

sub add
  { my( $ent_list, $ent)= @_;
    $ent_list->{entities}->{$ent->{name}}= $ent;
    $ent_list->{updated}=1;
    return $ent_list;
  }

sub ent
  { my( $ent_list, $ent_name)= @_;
    return $ent_list->{entities}->{$ent_name};
  }

# can be called with an entity or with an entity name
sub delete
  { my $ent_list= shift;
    if( isa( ref $_[0], 'XML::Twig::Entity'))
      { # the second arg is an entity
        my $ent= shift;
        delete $ent_list->{entities}->{$ent->{name}};
      }
    else
      { # the second arg was not entity, must be a string then
        my $name= shift;
        delete $ent_list->{entities}->{$name};
      }
    $ent_list->{updated}=1;
    return $ent_list;
  }

sub print
  { my ($ent_list, $fh)= @_;
    my $old_select= defined $fh ? select $fh : undef;

    foreach my $ent_name ( sort keys %{$ent_list->{entities}})
      { my $ent= $ent_list->{entities}->{$ent_name};
        # we have to test what the entity is or un-defined entities can creep in
        if( isa( $ent, 'XML::Twig::Entity')) { $ent->print(); }
      }
    select $old_select if( defined $old_select);
    return $ent_list;
  }

sub text
  { my ($ent_list)= @_;
    return join "\n", map { $ent_list->{entities}->{$_}->text} sort keys %{$ent_list->{entities}};
  }

# return the list of entity names 
sub entity_names
  { my $ent_list= shift;
    return (sort keys %{$ent_list->{entities}}) ;
  }


sub list
  { my ($ent_list)= @_;
    return map { $ent_list->{entities}->{$_} } sort keys %{$ent_list->{entities}};
  }

1;

######################################################################
package XML::Twig::Entity;
######################################################################

#*isa= *UNIVERSAL::isa;

sub new
  { my( $class, $name, $val, $sysid, $pubid, $ndata, $param)= @_;
    $class= ref( $class) || $class;

    my $self={};
    
    $self->{name}  = $name;
    $self->{val}   = $val   if( defined $val  );
    $self->{sysid} = $sysid if( defined $sysid);
    $self->{pubid} = $pubid if( defined $pubid);
    $self->{ndata} = $ndata if( defined $ndata);
    $self->{param} = $param if( defined $param);

    bless $self, $class;
    return $self;
  }


sub name  { return $_[0]->{name}; }
sub val   { return $_[0]->{val}; }
sub sysid { return defined( $_[0]->{sysid}) ? $_[0]->{sysid} : ''; }
sub pubid { return defined( $_[0]->{pubid}) ? $_[0]->{pubid} : ''; }
sub ndata { return defined( $_[0]->{ndata}) ? $_[0]->{ndata} : ''; }
sub param { return defined( $_[0]->{param}) ? $_[0]->{param} : ''; }


sub print
  { my ($ent, $fh)= @_;
    my $text= $ent->text;
    if( $fh) { print $fh $text . "\n"; }
    else     { print $text . "\n"; }
  }

sub sprint
  { my ($ent)= @_;
    return $ent->text;
  }

sub text
  { my ($ent)= @_;
    #warn "text called: '", $ent->_dump, "'\n";
    return '' if( !$ent->{name});
    my @tokens;
    push @tokens, '<!ENTITY';
   
    push @tokens, '%' if( $ent->{param});
    push @tokens, $ent->{name};

    if( defined $ent->{val} && !defined( $ent->{sysid}) && !defined($ent->{pubid}) )
      { push @tokens, _quoted_val( $ent->{val});
      }
    elsif( defined $ent->{sysid})
      { push @tokens, 'PUBLIC', _quoted_val( $ent->{pubid}) if( $ent->{pubid});
        push @tokens, 'SYSTEM' unless( $ent->{pubid});
        push @tokens, _quoted_val( $ent->{sysid}); 
        push @tokens, 'NDATA', $ent->{ndata} if( $ent->{ndata});
      }
    return join( ' ', @tokens) . '>';
  }

sub _quoted_val
  { my $q= $_[0]=~ m{"} ? q{'} : q{"};
    return qq{$q$_[0]$q};
  }

sub _dump
  { my( $ent)= @_; return join( " - ", map { "$_ => '$ent->{$_}'" } grep { defined $ent->{$_} } sort keys %$ent); }
                
1;

######################################################################
package XML::Twig::Elt;
######################################################################

use Carp;
*isa= *UNIVERSAL::isa;

my $CDATA_START    = "<![CDATA[";
my $CDATA_END      = "]]>";
my $PI_START       = "<?";
my $PI_END         = "?>";
my $COMMENT_START  = "<!--";
my $COMMENT_END    = "-->";

my $XMLNS_URI      = 'http://www.w3.org/2000/xmlns/';


BEGIN
  { # set some aliases for methods
    *tag           = *gi; 
    *name          = *gi; 
    *set_tag       = *set_gi; 
    *set_name      = *set_gi; 
    *find_nodes    = *get_xpath; # as in XML::DOM
    *findnodes     = *get_xpath; # as in XML::LibXML
    *field         = *first_child_text;
    *trimmed_field = *first_child_trimmed_text;
    *is_field      = *contains_only_text;
    *is            = *passes;
    *matches       = *passes;
    *has_child     = *first_child;
    *has_children  = *first_child;
    *all_children_pass = *all_children_are;
    *all_children_match= *all_children_are;
    *getElementsByTagName= *descendants;
    *find_by_tag_name= *descendants_or_self;
    *unwrap          = *erase;
    *inner_xml       = *xml_string;
    *outer_xml       = *sprint;
    *add_class       = *add_to_class;
  
    *first_child_is  = *first_child_matches;
    *last_child_is   = *last_child_matches;
    *next_sibling_is = *next_sibling_matches;
    *prev_sibling_is = *prev_sibling_matches;
    *next_elt_is     = *next_elt_matches;
    *prev_elt_is     = *prev_elt_matches;
    *parent_is       = *parent_matches;
    *child_is        = *child_matches;
    *inherited_att   = *inherit_att;

    *sort_children_by_value= *sort_children_on_value;

    *has_atts= *att_nb;

    # imports from XML::Twig
    *_is_fh= *XML::Twig::_is_fh;

    # XML::XPath compatibility
    *string_value       = *text;
    *toString           = *sprint;
    *getName            = *gi;
    *getRootNode        = *twig;  
    *getNextSibling     = *_next_sibling;
    *getPreviousSibling = *_prev_sibling;
    *isElementNode      = *is_elt;
    *isTextNode         = *is_text;
    *isPI               = *is_pi;
    *isPINode           = *is_pi;
    *isProcessingInstructionNode= *is_pi;
    *isComment          = *is_comment;
    *isCommentNode      = *is_comment;
    *getTarget          = *target;
    *getFirstChild      = *_first_child;
    *getLastChild      = *_last_child;

    # try using weak references
    # test whether we can use weak references
    { local $SIG{__DIE__};
      if( eval 'require Scalar::Util' && defined( &Scalar::Util::weaken) )
        { import Scalar::Util qw(weaken); }
      elsif( eval 'require WeakRef')
        { import WeakRef; }
    }
}

 
# can be called as XML::Twig::Elt->new( [[$gi, $atts, [@content]])
# - gi is an optional gi given to the element
# - $atts is a hashref to attributes for the element
# - @content is an optional list of text and elements that will
#   be inserted under the element 
sub new 
  { my $class= shift;
    $class= ref $class || $class;
    my $elt  = {};
    bless ($elt, $class);

    return $elt unless @_;

    # if a gi is passed then use it
    my $gi= shift;
    $elt->{gi}=$XML::Twig::gi2index{$gi} or $elt->set_gi( $gi);


    my $atts= ref $_[0] eq 'HASH' ? shift : undef;

    if( $atts && defined $atts->{$CDATA})
      { delete $atts->{$CDATA};

        my $cdata= $class->new( $CDATA => @_);
        return $class->new( $gi, $atts, $cdata);
      }

    if( $gi eq $PCDATA)
      { if( grep { ref $_ } @_) { croak "element $PCDATA can only be created from text"; }
        $elt->_set_pcdata( join( '', @_)); 
      }
    elsif( $gi eq $ENT)
      { $elt->{ent}=  shift; }
    elsif( $gi eq $CDATA)
      { if( grep { ref $_ } @_) { croak "element $CDATA can only be created from text"; }
        $elt->_set_cdata( join( '', @_)); 
      }
    elsif( $gi eq $COMMENT)
      { if( grep { ref $_ } @_) { croak "element $COMMENT can only be created from text"; }
        $elt->_set_comment( join( '', @_)); 
      }
    elsif( $gi eq $PI)
      { if( grep { ref $_ } @_) { croak "element $PI can only be created from text"; }
        $elt->_set_pi( shift, join( '', @_));
      }
    else
      { # the appdynamics of the arguments are the content of the element
        if( @_)
          { $elt->set_content( @_); }
        else
          { $elt->{empty}=  1;    }
      }

    if( $atts)
      { # the attribute hash can be used to pass the asis status 
        if( defined $atts->{$ASIS})  { $elt->set_asis(  $atts->{$ASIS} ); delete $atts->{$ASIS};  }
        if( defined $atts->{$EMPTY}) { $elt->{empty}=  $atts->{$EMPTY}; delete $atts->{$EMPTY}; }
        if( keys %$atts) { $elt->set_atts( $atts); }
        $elt->_set_id( $atts->{$ID}) if( $atts->{$ID});
      }

    return $elt;
  }

# optimized version of $elt->new( PCDATA, $text);
sub _new_pcdata
  { my $class= $_[0];
    $class= ref $class || $class;
    my $elt  = {};
    bless $elt, $class;
    $elt->{gi}=$XML::Twig::gi2index{$PCDATA} or $elt->set_gi( $PCDATA);
    $elt->_set_pcdata( $_[1]);
    return $elt;
  }
    
# this function creates an XM:::Twig::Elt from a string
# it is quite clumsy at the moment, as it just creates a
# new twig then returns its root
# there might also be memory leaks there
# additional arguments are passed to new XML::Twig
sub parse
  { my $class= shift;
    if( ref( $class)) { $class= ref( $class); }
    my $string= shift;
    my %args= @_;
    my $t= XML::Twig->new(%args);
    $t->parse( $string);
    my $elt= $t->root;
    # clean-up the node 
    delete $elt->{twig};         # get rid of the twig data
    delete $elt->{twig_current}; # better get rid of this too
    if( $t->{twig_id_list}) { $elt->{twig_id_list}= $t->{twig_id_list}; }
    return $elt;
  }
   
sub set_inner_xml
  { my( $elt, $xml, @args)= @_;
    my $new_elt= $elt->parse( "<dummy>$xml</dummy>", @args);
    $elt->cut_children;
    $new_elt->paste_first_child( $elt);
    $new_elt->erase;
    return $elt;
  }
 
sub set_outer_xml
  { my( $elt, $xml, @args)= @_;
    my $new_elt= $elt->parse( "<dummy>$xml</dummy>", @args);
    $elt->cut_children;
    $new_elt->replace( $elt);
    $new_elt->erase;
    return $new_elt;
  }
  
 
sub set_inner_html
  { my( $elt, $html)= @_;
    my $t= XML::Twig->new->parse_html( "<html>$html</html>");
    my $new_elt= $t->root;
    if( $elt->tag eq 'head')
      { $new_elt->first_child( 'head')->unwrap;
        $new_elt->first_child( 'body')->cut;
      }
    elsif( $elt->tag ne 'html')
      { $new_elt->first_child( 'head')->cut;
        $new_elt->first_child( 'body')->unwrap;
      }
    $new_elt->cut;
    $elt->cut_children;
    $new_elt->paste_first_child( $elt);
    $new_elt->erase;
    return $elt;
  }

sub set_gi 
  { my ($elt, $gi)= @_;
    unless( defined $XML::Twig::gi2index{$gi})
      { # new gi, create entries in %gi2index and @index2gi
        push  @XML::Twig::index2gi, $gi;
        $XML::Twig::gi2index{$gi}= $#XML::Twig::index2gi;
      }
    $elt->{gi}= $XML::Twig::gi2index{$gi};
    return $elt; 
  }

sub gi  { return $XML::Twig::index2gi[$_[0]->{gi}]; }

sub local_name 
  { my $elt= shift;
    return _local_name( $XML::Twig::index2gi[$elt->{'gi'}]);
  }

sub ns_prefix
  { my $elt= shift;
    return _ns_prefix( $XML::Twig::index2gi[$elt->{'gi'}]);
  }

# namespace prefix for any qname (can be used for elements or attributes)
sub _ns_prefix
  { my $qname= shift;
    if( $qname=~ m{^([^:]*):})
      { return $1; }
    else
      { return( ''); } # should it be '' ?
  }

# local name for any qname (can be used for elements or attributes)
sub _local_name
  { my $qname= shift;
    (my $local= $qname)=~ s{^[^:]*:}{};
    return $local;
  }

#sub get_namespace
sub namespace ## no critic (Subroutines::ProhibitNestedSubs);
  { my $elt= shift;
    my $prefix= defined $_[0] ? shift() : $elt->ns_prefix;
    my $ns_att= $prefix ? "xmlns:$prefix" : "xmlns";
    my $expanded= $DEFAULT_NS{$prefix} || $elt->_inherit_att_through_cut( $ns_att) || '';
    return $expanded;
  }

sub declare_missing_ns ## no critic (Subroutines::ProhibitNestedSubs);
  { my $root= shift;
    my %missing_prefix;
    my $map= $root->_current_ns_prefix_map;

    foreach my $prefix (keys %$map)
      { my $prefix_att= $prefix eq '#default' ? 'xmlns' : "xmlns:$prefix";
        if( ! $root->{'att'}->{$prefix_att}) 
          { $root->set_att( $prefix_att => $map->{$prefix}); }
      }
    return $root;
  }

sub _current_ns_prefix_map
  { my( $elt)= shift;
    my $map;
    while( $elt)
      { foreach my $att ($elt->att_names)
          { my $prefix= $att eq 'xmlns'        ? '#default'
                      : $att=~ m{^xmlns:(.*)$} ? $1
                      : next
                      ;
            if( ! exists $map->{$prefix}) { $map->{$prefix}= $elt->{'att'}->{$att}; }
          }
        $elt= $elt->{parent} || $elt->former_parent;
      }
    return $map;
  }
 
sub set_ns_decl
  { my( $elt, $uri, $prefix)= @_;
    my $ns_att=  $prefix ? "xmlns:$prefix" : 'xmlns';
    $elt->set_att( $ns_att => $uri);
    return $elt;
  }

sub set_ns_as_default
  { my( $root, $uri)= @_;
    my @ns_decl_to_remove;
    foreach my $elt ($root->descendants_or_self)
      { if( $elt->_ns_prefix && $elt->namespace eq $uri) 
          { $elt->set_tag( $elt->local_name); }
        # store any namespace declaration for that uri
        foreach my $ns_decl (grep { $_=~ m{xmlns(:|$)} && $elt->{'att'}->{$_} eq $uri } $elt->att_names)
          { push @ns_decl_to_remove, [$elt, $ns_decl]; }
      }
    $root->set_ns_decl( $uri);
    # now remove the ns declarations (if done earlier then descendants of an element with the ns declaration
    # are not considered being in the namespace
    foreach my $ns_decl_to_remove ( @ns_decl_to_remove)
      { my( $elt, $ns_decl)= @$ns_decl_to_remove;
        $elt->del_att( $ns_decl);
      }
    
    return $root;
  }
     


# return #ELT for an element and #PCDATA... for others
sub get_type
  { my $gi_nb= $_[0]->{gi}; # the number, not the string
    return $ELT if( $gi_nb >= $XML::Twig::SPECIAL_GI);
    return $_[0]->gi;
  }

# return the gi if it's a "real" element, 0 otherwise
sub is_elt
  { if(  $_[0]->{gi} >=  $XML::Twig::SPECIAL_GI)
     { return $_[0]->gi; }
    else
      { return 0; }
  }


sub is_pcdata
  { my $elt= shift;
    return (exists $elt->{'pcdata'});
  }

sub is_cdata
  { my $elt= shift;
    return (exists $elt->{'cdata'});
  }

sub is_pi
  { my $elt= shift;
    return (exists $elt->{'target'});
  }

sub is_comment
  { my $elt= shift;
    return (exists $elt->{'comment'});
  }

sub is_ent
  { my $elt= shift;
    return (exists $elt->{ent} || $elt->{ent_name});
  } 


sub is_text
  { my $elt= shift;
    return (exists( $elt->{'pcdata'}) || (exists $elt->{'cdata'}));
  }

sub is_empty
  { return $_[0]->{empty} || 0; }

sub set_empty
  { $_[0]->{empty}= defined( $_[1]) ? $_[1] : 1; return $_[0]; }

sub set_not_empty
  { delete $_[0]->{empty} if( $_[0]->{'empty'}); return $_[0]; }


sub set_asis
  { my $elt=shift;

    foreach my $descendant ($elt, $elt->_descendants )
      { $descendant->{asis}= 1;
        if( (exists $descendant->{'cdata'}))
          { $descendant->{gi}=$XML::Twig::gi2index{$PCDATA} or $descendant->set_gi( $PCDATA);
            $descendant->_set_pcdata( $descendant->{cdata});
          }

      }
    return $elt;
  }

sub set_not_asis
  { my $elt=shift;
    foreach my $descendant ($elt, $elt->descendants)
      { delete $descendant->{asis} if $descendant->{asis};}
    return $elt;
  }

sub is_asis
  { return $_[0]->{asis}; }

sub closed 
  { my $elt= shift;
    my $t= $elt->twig || return;
    my $curr_elt= $t->{twig_current};
    return 1 unless( $curr_elt);
    return $curr_elt->in( $elt);
  }

sub set_pcdata 
  { my( $elt, $pcdata)= @_;
  
    if( $elt->{extra_data_in_pcdata})
      { _try_moving_extra_data( $elt, $pcdata);
      }
    $elt->{pcdata}= $pcdata;
    return $elt; 
  }

sub _extra_data_in_pcdata      { return $_[0]->{extra_data_in_pcdata}; }
sub _set_extra_data_in_pcdata  { $_[0]->{extra_data_in_pcdata}= $_[1]; return $_[0]; }
sub _del_extra_data_in_pcdata  { delete $_[0]->{extra_data_in_pcdata}; return $_[0]; }
sub _unshift_extra_data_in_pcdata { unshift @{shift()->{extra_data_in_pcdata}}, { text => shift(), offset => shift() }; }
sub _push_extra_data_in_pcdata    { push @{shift()->{extra_data_in_pcdata}},    { text => shift(), offset => shift() }; }

sub _extra_data_before_end_tag     { return $_[0]->{extra_data_before_end_tag} || ''; }
sub _set_extra_data_before_end_tag { $_[0]->{extra_data_before_end_tag}= $_[1]; return $_[0]}
sub _del_extra_data_before_end_tag { delete $_[0]->{extra_data_before_end_tag}; return $_[0]}
sub _prefix_extra_data_before_end_tag 
  { my( $elt, $data)= @_;
    if($elt->{extra_data_before_end_tag})
      { $elt->{extra_data_before_end_tag}= $data . $elt->{extra_data_before_end_tag}; }
    else  
      { $elt->{extra_data_before_end_tag}= $data; }
    return $elt;
  }

# internal, in cases where we know there is no extra_data (inlined anyway!)
sub _set_pcdata { $_[0]->{pcdata}= $_[1]; }

# try to figure out if we can keep the extra_data around
sub _try_moving_extra_data
  { my( $elt, $modified)=@_;
    my $initial= $elt->{pcdata};
    my $cpis= $elt->{extra_data_in_pcdata};

    if( (my $offset= index( $modified, $initial)) != -1) 
      { # text has been added
        foreach (@$cpis) { $_->{offset}+= $offset; }
      }
    elsif( ($offset= index( $initial, $modified)) != -1)
      { # text has been cut
        my $len= length( $modified);
        foreach my $cpi (@$cpis) { $cpi->{offset} -= $offset; }
        $elt->_set_extra_data_in_pcdata( [ grep { $_->{offset} >= 0 && $_->{offset} < $len } @$cpis ]);
      } 
    else
      {    _match_extra_data_words( $elt, $initial, $modified)
        || _match_extra_data_chars( $elt, $initial, $modified)
        || $elt->_del_extra_data_in_pcdata;
      }
  }

sub _match_extra_data_words
  { my( $elt, $initial, $modified)= @_;
    my @initial= split /\b/, $initial; 
    my @modified= split /\b/, $modified;
       
    return _match_extra_data( $elt, length( $initial), \@initial, \@modified);
  }
  
sub _match_extra_data_chars
  { my( $elt, $initial, $modified)= @_;
    my @initial= split //, $initial; 
    my @modified= split //, $modified;
       
    return _match_extra_data( $elt, length( $initial), \@initial, \@modified);
  }

sub _match_extra_data
  { my( $elt, $length, $initial, $modified)= @_;
        
    my $cpis= $elt->{extra_data_in_pcdata};

    if( @$initial <= @$modified)
      { 
        my( $ok, $positions, $offsets)= _pos_offset( $initial, $modified);
        if( $ok) 
          { my $offset=0;
            my $pos= shift @$positions;
            foreach my $cpi (@$cpis)
              { while( $cpi->{offset} >= $pos)
                  { $offset= shift @$offsets; 
                    $pos= shift @$positions || $length +1;
                  }
                $cpi->{offset} += $offset;
              }
            return 1;
          }
      }
    else
      { my( $ok, $positions, $offsets)= _pos_offset( $modified, $initial);
        if( $ok)
          { #print STDERR "pos:    ", join( ':', @$positions), "\n",
            #             "offset: ", join( ':', @$offsets), "\n";
            my $offset=0;
            my $pos= shift @$positions;
            my $prev_pos= 0;
            
            foreach my $cpi (@$cpis)
              { while( $cpi->{offset} >= $pos)
                  { $offset= shift @$offsets;
                    $prev_pos= $pos;
                    $pos= shift @$positions || $length +1;
                  }
                $cpi->{offset} -= $offset;
                if( $cpi->{offset} < $prev_pos) { delete $cpi->{text}; }
              }
            $elt->_set_extra_data_in_pcdata( [ grep { exists $_->{text} } @$cpis ]);
            return 1;
          }
      }
    return 0;
  }

          
sub _pos_offset
  { my( $short, $long)= @_;
    my( @pos, @offset);
    my( $s_length, $l_length)=(0,0);
    while (@$short)
      { my $s_word= shift @$short;
        my $l_word= shift @$long;
        if( $s_word ne $l_word)
          { while( @$long && $s_word ne $l_word)
              { $l_length += length( $l_word);
                $l_word= shift @$long;
              }
            if( !@$long && $s_word ne $l_word) { return 0; }
            push @pos, $s_length;
            push @offset, $l_length - $s_length;
          }
        my $length= length( $s_word);
        $s_length += $length;
        $l_length += $length;
      }
    return( 1, \@pos, \@offset);
  }

sub append_pcdata
  { $_[0]->{'pcdata'}.= $_[1];
    return $_[0]; 
  }

sub pcdata        { return $_[0]->{pcdata}; }


sub append_extra_data 
  {  $_[0]->{extra_data}.= $_[1];
     return $_[0]; 
  }
  
sub set_extra_data 
  { $_[0]->{extra_data}= $_[1];
    return $_[0]; 
  }
sub extra_data { return $_[0]->{extra_data} || ''; }

sub set_target 
  { my( $elt, $target)= @_;
    $elt->{target}= $target;
    return $elt; 
  }
sub target { return $_[0]->{target}; }

sub set_data 
  { $_[0]->{'data'}= $_[1]; 
    return $_[0];
  }
sub data { return $_[0]->{data}; }

sub set_pi
  { my $elt= shift;
    unless( $elt->{gi} == $XML::Twig::gi2index{$PI})
      { $elt->cut_children;
        $elt->{gi}=$XML::Twig::gi2index{$PI} or $elt->set_gi( $PI);
      }
    return $elt->_set_pi( @_);
  }

sub _set_pi
  { $_[0]->set_target( $_[1]);
    $_[0]->{data}=  $_[2];
    return $_[0]; 
  }

sub pi_string { my $string= $PI_START . $_[0]->{target};
                my $data= $_[0]->{data};
                if( defined( $data) && $data ne '') { $string .= " $data"; }
                $string .= $PI_END ;
                return $string;
              }

sub set_comment
  { my $elt= shift;
    unless( $elt->{gi} == $XML::Twig::gi2index{$COMMENT})
      { $elt->cut_children;
        $elt->{gi}=$XML::Twig::gi2index{$COMMENT} or $elt->set_gi( $COMMENT);
      }
    return $elt->_set_comment( @_);
  }

sub _set_comment   { $_[0]->{comment}= $_[1]; return $_[0]; }
sub comment        { return $_[0]->{comment}; }
sub comment_string { return $COMMENT_START . _comment_escaped_string( $_[0]->{comment}) . $COMMENT_END; }
# comments cannot start or end with 
sub _comment_escaped_string 
  { my( $c)= @_;
    $c=~ s{^-}{ -};  
    $c=~ s{-$}{- };
    $c=~ s{--}{- -}g;
    return $c;
  }

sub set_ent  { $_[0]->{ent}= $_[1]; return $_[0]; }
sub ent      { return $_[0]->{ent}; }
sub ent_name { return substr( $_[0]->{ent}, 1, -1);}

sub set_cdata 
  { my $elt= shift;
    unless( $elt->{gi} == $XML::Twig::gi2index{$CDATA})
      { $elt->cut_children;
        $elt->insert_new_elt( first_child => $CDATA, @_);
        return $elt;
      }
    return $elt->_set_cdata( @_);
  }
  
sub _set_cdata 
  { $_[0]->{cdata}= $_[1]; 
    return $_[0];
  }

sub append_cdata
  { $_[0]->{cdata}.= $_[1]; 
    return $_[0];
  }
sub cdata { return $_[0]->{cdata}; }


sub contains_only_text
  { my $elt= shift;
    return 0 unless $elt->is_elt;
    foreach my $child ($elt->_children)
      { return 0 if $child->is_elt; }
    return $elt;
  } 
  
sub contains_only
  { my( $elt, $exp)= @_;
    my @children= do { my $elt= $elt; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; };
    foreach my $child (@children)
      { return 0 unless $child->is( $exp); }
    return @children || 1;
  } 

sub contains_a_single
  { my( $elt, $exp)= @_;
    my $child= $elt->{first_child} or return 0;
    return 0 unless $child->passes( $exp);
    return 0 if( $child->{next_sibling});
    return $child;
  } 


sub root 
  { my $elt= shift;
    while( $elt->{parent}) { $elt= $elt->{parent}; }
    return $elt;
  }

sub _root_through_cut
  { my $elt= shift;
    while( $elt->{parent} || $elt->former_parent) { $elt= $elt->{parent} || $elt->former_parent; }
    return $elt;
  }

sub twig 
  { my $elt= shift;
    my $root= $elt->root;
    return $root->{twig};
  }

sub _twig_through_cut
  { my $elt= shift;
    my $root= $elt->_root_through_cut;
    return $root->{twig};
  }


# used for navigation
# returns undef or the element, depending on whether $elt passes $cond
# $cond can be
# - empty: the element passes the condition
# - ELT ('#ELT'): the element passes the condition if it is a "real" element
# - TEXT ('#TEXT'): the element passes if it is a CDATA or PCDATA element
# - a string with an XPath condition (only a subset of XPath is actually
#   supported).
# - a regexp: the element passes if its gi matches the regexp
# - a code ref: the element passes if the code, applied on the element,
#   returns true

my %cond_cache; # expression => coderef

sub reset_cond_cache { %cond_cache=(); }

{ 
   sub _install_cond
    { my $cond= shift;
      my $test;
      my $init=''; 

      my $original_cond= $cond;

      my $not= ($cond=~ s{^\s*!}{}) ? '!' : '';

      if( ref $cond eq 'CODE') { return $cond; }
    
      if( ref $cond eq 'Regexp')
        { $test = qq{(\$_[0]->gi=~ /$cond/)}; }
      else
        { my @tests;
          while( $cond)
            { 
              # the condition is a string
              if( $cond=~ s{$ELT$SEP}{})     
                { push @tests, qq{\$_[0]->is_elt}; }
              elsif( $cond=~ s{$TEXT$SEP}{}) 
                { push @tests, qq{\$_[0]->is_text}; }
              elsif( $cond=~ s{^\s*($REG_NAME_WC)$SEP}{})                  
                { push @tests, _gi_test( $1); } 
              elsif( $cond=~ s{^\s*($REG_REGEXP)$SEP}{})
                { # /regexp/
                  push @tests, qq{ \$_[0]->gi=~ $1 }; 
                }
              elsif( $cond=~ s{^\s*($REG_NAME_WC)?\s*  # $1
                               \[\s*(-?)\s*(\d+)\s*\]  #   [$2]
                               $SEP}{}xo
                   )
                { my( $gi, $neg, $index)= ($1, $2, $3);
                  my $siblings= $neg ? q{$_[0]->_next_siblings} : q{$_[0]->_prev_siblings};
                  if( $gi && ($gi ne '*')) 
                    #{ $test= qq{((\$_[0]->gi eq "$gi") && (scalar( grep { \$_->gi eq "$gi" } $siblings) + 1 == $index))}; }
                    { push @tests, _and( _gi_test( $gi), qq{ (scalar( grep { \$_->gi eq "$gi" } $siblings) + 1 == $index)}); }
                  else
                    { push @tests, qq{(scalar( $siblings) + 1 == $index)}; }
                }
              elsif( $cond=~ s{^\s*($REG_NAME_WC?)\s*($REG_PREDICATE)$SEP}{})
                { my( $gi, $predicate)= ( $1, $2);
                  push @tests, _and( _gi_test( $gi), _parse_predicate_in_step( $predicate));
                }
              elsif( $cond=~ s{^\s*($REG_NAKED_PREDICATE)$SEP}{})
                { push @tests,   _parse_predicate_in_step( $1); }
              else
                { croak "wrong navigation condition '$original_cond' ($@)"; }
            }
           $test= @tests > 1 ? '(' . join( '||', map { "($_)" } @tests) . ')' : $tests[0];
        }

      #warn "init: '$init' - test: '$test'\n";

      my $sub= qq{sub { $NO_WARNINGS; $init; return $not($test) ? \$_[0] : undef; } };
      my $s= eval $sub; 
      #warn "cond: $cond\n$sub\n";
      if( $@) 
        { croak "wrong navigation condition '$original_cond' ($@);" }
      return $s;
    }

  sub _gi_test
    { my( $full_gi)= @_;

      # optimize if the gi exists, including the case where the gi includes a dot
      my $index= $XML::Twig::gi2index{$full_gi};
      if( $index) { return qq{\$_[0]->{gi} == $index}; }

      my( $gi, $class)= $full_gi=~ m{^(.*?)(?:\.([^.]*))?$};

      my $gi_test='';
      if( $gi && $gi ne '*' )
        { # 2 options, depending on whether the gi exists in gi2index
          # start optimization
          my $index= $XML::Twig::gi2index{$gi};
          if( $index)
            { # the gi exists, use its index as a faster shortcut
              $gi_test = qq{\$_[0]->{gi} == $index};
            }
          else
          # end optimization
            { # it does not exist (but might be created later), compare the strings
              $gi_test = qq{ \$_[0]->gi eq "$gi"}; 
            }
        }
      else
        { $gi_test= 1; }

      my $class_test='';
      #warn "class: '$class'";
      if( $class)
        { $class_test = qq{ defined( \$_[0]->{att}->{class}) && \$_[0]->{att}->{class}=~ m{\\b$class\\b} }; }
      #warn "gi_test: '$gi_test' - class_test: '$class_test' returning ",  _and( $gi_test, $class_test);
      return _and( $gi_test, $class_test);
  }


  # input: the original predicate
  sub _parse_predicate_in_step
    { my $cond= shift; 
      my %PERL_ALPHA_TEST= ( '=' => ' eq ', '!=' => ' ne ', '>' => ' gt ', '>=' => ' ge ', '<' => ' lt ', '<=' => ' le ');

      $cond=~ s{^\s*\[\s*}{};
      $cond=~ s{\s*\]\s*$}{};
      $cond=~ s{(   ($REG_STRING|$REG_REGEXP)            # strings or regexps
                   |\@($REG_NAME)(?=\s*(?:[><=!]|!~|=~)) # @att (followed by a comparison operator)
                   |\@($REG_NAME)                        # @att (not followed by a comparison operator)
                   |=~|!~                                # matching operators
                   |([><]=?|=|!=)(?=\s*[\d+-])           # test before a number
                   |([><]=?|=|!=)                        # test, other cases
                   |($REG_FUNCTION)                      # no arg functions
                   # this bit is a mess, but it is the only solution with this half-baked parser
                   |((?:string|text)\(\s*$REG_NAME\s*\)\s*$REG_MATCH\s*$REG_REGEXP) # string( child) =~ /regexp/
                   |((?:string|text)\(\s*$REG_NAME\s*\)\s*!?=\s*$REG_VALUE)         # string( child) = "value" (or !=)
                   |((?:string|text)\(\s*$REG_NAME\s*\)\s*[<>]=?\s*$REG_VALUE)      # string( child) > "value"
                   |(and|or)
                )}
               { my( $token, $string, $att, $bare_att, $num_test, $alpha_test, $func, $string_regexp, $string_eq, $string_test, $and_or)
                 = ( $1,     $2,      $3,   $4,        $5,        $6,          $7,    $8,             $9,         $10,          $11);
      
                 if( defined $string)   { $token }
                 elsif( $att)           { "( \$_[0]->{att} && exists( \$_[0]->{att}->{'$att'}) && \$_[0]->{att}->{'$att'})"; }
                 elsif( $bare_att)      { "(\$_[0]->{att} && defined( \$_[0]->{att}->{'$bare_att'}))"; }
                 elsif( $num_test && ($num_test eq '=') ) { "==" } # others tests are unchanged
                 elsif( $alpha_test)    { $PERL_ALPHA_TEST{$alpha_test} }
                 elsif( $func && $func=~ m{^(?:string|text)})
                                        { "\$_[0]->text"; }
                 elsif( $string_regexp && $string_regexp =~ m{(?:string|text)\(\s*($REG_NAME)\s*\)\s*($REG_MATCH)\s*($REG_REGEXP)})
                                        { "(XML::Twig::_first_n { (\$_->gi eq '$1') && (\$_->text $2 $3) } 1, \$_[0]->_children)"; }
                 elsif( $string_eq     && $string_eq     =~ m{(?:string|text)\(\s*($REG_NAME)\s*\)\s*(!?=)\s*($REG_VALUE)})
                                        {"(XML::Twig::_first_n { (\$_->gi eq '$1') && (\$_->text $PERL_ALPHA_TEST{$2} $3) } 1, \$_[0]->_children)"; }
                 elsif( $string_test   && $string_test   =~ m{(?:string|text)\(\s*($REG_NAME)\s*\)\s*([<>]=?)\s*($REG_VALUE)})
                                        { "(XML::Twig::_first_n { (\$_->gi eq '$1') && (\$_->text $2 $3) } 1, \$_[0]->_children)"; }
                 elsif( $and_or)        { $and_or eq 'and' ? '&&' : '||' ; }
                 else                   { $token; }
               }gexs;
      return "($cond)";
    }
  

  sub _op
    { my $op= shift;
      if(    $op eq '=')  { $op= 'eq'; }
      elsif( $op eq '!=') { $op= 'ne'; }
      return $op;
    }

  sub passes
    { my( $elt, $cond)= @_;
      return $elt unless $cond;
      my $sub= ($cond_cache{$cond} ||= _install_cond( $cond));
      return $sub->( $elt);
    }
}

sub set_parent 
  { $_[0]->{parent}= $_[1];
    if( $XML::Twig::weakrefs) { weaken( $_[0]->{parent}); }
  }

sub parent
  { my $elt= shift;
    my $cond= shift || return $elt->{parent};
    do { $elt= $elt->{parent} || return; } until ( $elt->passes( $cond));
    return $elt;
  }

sub set_first_child 
  { $_[0]->{'first_child'}= $_[1]; 
  }

sub first_child
  { my $elt= shift;
    my $cond= shift || return $elt->{first_child};
    my $child= $elt->{first_child};
    my $test_cond= ($cond_cache{$cond} ||= _install_cond( $cond));
    while( $child && !$test_cond->( $child)) 
       { $child= $child->{next_sibling}; }
    return $child;
  }
  
sub _first_child   { return $_[0]->{first_child};  }
sub _last_child    { return $_[0]->{last_child};   }
sub _next_sibling  { return $_[0]->{next_sibling}; }
sub _prev_sibling  { return $_[0]->{prev_sibling}; }
sub _parent        { return $_[0]->{parent};       }
sub _next_siblings { my $elt= shift; my @siblings; while( $elt= $elt->{next_sibling}) { push @siblings, $elt; } return @siblings; }
sub _prev_siblings { my $elt= shift; my @siblings; while( $elt= $elt->{prev_sibling}) { push @siblings, $elt; } return @siblings; }

# sets a field
# arguments $record, $cond, @content
sub set_field
  { my $record = shift;
    my $cond = shift;
    my $child= $record->first_child( $cond);
    if( $child)
      { $child->set_content( @_); }
    else
      { if( $cond=~ m{^\s*($REG_NAME)})
          { my $gi= $1;
            $child= $record->insert_new_elt( last_child => $gi, @_); 
          }
        else
          { croak "can't create a field name from $cond"; }
      } 
    return $child;
  }

sub set_last_child 
  { $_[0]->{'last_child'}= $_[1];
    if( $XML::Twig::weakrefs) { weaken( $_[0]->{'last_child'}); }
  }

sub last_child
  { my $elt= shift;
    my $cond= shift || return $elt->{last_child};
    my $test_cond= ($cond_cache{$cond} ||= _install_cond( $cond));
    my $child= $elt->{last_child};
    while( $child && !$test_cond->( $child) )
      { $child= $child->{prev_sibling}; }
    return $child
  }


sub set_prev_sibling 
  { $_[0]->{'prev_sibling'}= $_[1]; 
    if( $XML::Twig::weakrefs) { weaken( $_[0]->{'prev_sibling'}); } 
  }

sub prev_sibling
  { my $elt= shift;
    my $cond= shift || return $elt->{prev_sibling};
    my $test_cond= ($cond_cache{$cond} ||= _install_cond( $cond));
    my $sibling= $elt->{prev_sibling};
    while( $sibling && !$test_cond->( $sibling) )
          { $sibling= $sibling->{prev_sibling}; }
    return $sibling;
  }

sub set_next_sibling { $_[0]->{'next_sibling'}= $_[1]; }

sub next_sibling
  { my $elt= shift;
    my $cond= shift || return $elt->{next_sibling};
    my $test_cond= ($cond_cache{$cond} ||= _install_cond( $cond));
    my $sibling= $elt->{next_sibling};
    while( $sibling && !$test_cond->( $sibling) )
          { $sibling= $sibling->{next_sibling}; }
    return $sibling;
  }

# methods dealing with the class attribute, convenient if you work with xhtml
sub class   {   $_[0]->{att}->{class}; }
# lvalue version of class. separate from class to avoid problem like RT#
sub lclass     
          :lvalue    # > perl 5.5
  { $_[0]->{att}->{class}; }

sub set_class { my( $elt, $class)= @_; $elt->set_att( class => $class); }

# adds a class to an element
sub add_to_class
  { my( $elt, $new_class)= @_;
    return $elt unless $new_class;
    my $class= $elt->class;
    my %class= $class ? map { $_ => 1 } split /\s+/, $class : ();
    $class{$new_class}= 1;
    $elt->set_class( join( ' ', sort keys %class));
  }

sub remove_class
  { my( $elt, $class_to_remove)= @_;
    return $elt unless $class_to_remove;
    my $class= $elt->class;
    my %class= $class ? map { $_ => 1 } split /\s+/, $class : ();
    delete $class{$class_to_remove};
    $elt->set_class( join( ' ', sort keys %class));
  }

sub att_to_class      { my( $elt, $att)= @_; $elt->set_class( $elt->{'att'}->{$att}); }
sub add_att_to_class  { my( $elt, $att)= @_; $elt->add_to_class( $elt->{'att'}->{$att}); }
sub move_att_to_class { my( $elt, $att)= @_; $elt->add_to_class( $elt->{'att'}->{$att});
                        $elt->del_att( $att); 
                      }
sub tag_to_class      { my( $elt)= @_; $elt->set_class( $elt->tag);    }
sub add_tag_to_class  { my( $elt)= @_; $elt->add_to_class( $elt->tag); }
sub set_tag_class     { my( $elt, $new_tag)= @_; $elt->add_tag_to_class; $elt->set_tag( $new_tag); }

sub tag_to_span       
  { my( $elt)= @_; 
    $elt->set_class( $elt->tag) unless( $elt->tag eq 'span' && $elt->class); # set class to span unless it would mean replacing it with span
    $elt->set_tag( 'span'); 
  }

sub tag_to_div    
  { my( $elt)= @_; 
    $elt->set_class( $elt->tag) unless( $elt->tag eq 'div' && $elt->class); # set class to div unless it would mean replacing it with div
    $elt->set_tag( 'div');
  }

sub in_class          
  { my( $elt, $class)= @_;
    my $elt_class= $elt->class;
    return unless( defined $elt_class);
    return $elt->class=~ m{(?:^|\s)\Q$class\E(?:\s|$)} ? $elt : 0;
  }


# get or set all attributes
# argument can be a hash or a hashref
sub set_atts 
  { my $elt= shift;
    my %atts;
    tie %atts, 'Tie::IxHash' if( keep_atts_order());
    %atts= ( (ref( $_[0] || '') eq 'HASH') || isa( $_[0] || '', 'HASH')) ? %{$_[0]} : @_;
    $elt->{att}= \%atts;
    if( exists $atts{$ID}) { $elt->_set_id( $atts{$ID}); }
    return $elt;
  }

sub atts      { return $_[0]->{att};                }
sub att_names { return (sort keys %{$_[0]->{att}}); }
sub del_atts  { $_[0]->{att}={}; return $_[0];      }

# get or set a single attribute (set works for several atts)
sub set_att 
  { my $elt= shift;

    if( $_[0] && ref( $_[0]) && !$_[1]) 
      { croak "improper call to set_att, usage is \$elt->set_att( att1 => 'val1', att2 => 'val2',...)"; }
    
    unless( $elt->{att})
      { $elt->{att}={};
        tie %{$elt->{att}}, 'Tie::IxHash' if( keep_atts_order());
      }

    while(@_) 
      { my( $att, $val)= (shift, shift);
        $elt->{att}->{$att}= $val;
        if( $att eq $ID) { $elt->_set_id( $val); } 
      }
    return $elt;
  }
 
sub att {  $_[0]->{att}->{$_[1]}; }
# lvalue version of att. separate from class to avoid problem like RT#
sub latt 
          :lvalue    # > perl 5.5
  { $_[0]->{att}->{$_[1]}; }

sub del_att 
  { my $elt= shift;
    while( @_) { delete $elt->{'att'}->{shift()}; }
    return $elt;
  }

sub att_exists { return exists  $_[0]->{att}->{$_[1]}; }

# delete an attribute from all descendants of an element
sub strip_att
  { my( $elt, $att)= @_;
    $_->del_att( $att) foreach ($elt->descendants_or_self( qq{*[\@$att]}));
    return $elt;
  }

sub change_att_name
  { my( $elt, $old_name, $new_name)= @_;
    my $value= $elt->{'att'}->{$old_name};
    return $elt unless( defined $value);
    $elt->del_att( $old_name)
        ->set_att( $new_name => $value);
    return $elt;
  }

sub lc_attnames
  { my $elt= shift;
    foreach my $att ($elt->att_names)
      { if( $att ne lc $att) { $elt->change_att_name( $att, lc $att); } }
    return $elt;
  }

sub set_twig_current { $_[0]->{twig_current}=1; }
sub del_twig_current { delete $_[0]->{twig_current}; }


# get or set the id attribute
sub set_id 
  { my( $elt, $id)= @_;
    $elt->del_id() if( exists $elt->{att}->{$ID});
    $elt->set_att($ID, $id); 
    $elt->_set_id( $id);
    return $elt;
  }

# only set id, does not update the attribute value
sub _set_id
  { my( $elt, $id)= @_;
    my $t= $elt->twig || $elt;
    $t->{twig_id_list}->{$id}= $elt;
    if( $XML::Twig::weakrefs) { weaken(  $t->{twig_id_list}->{$id}); }
    return $elt;
  }

sub id { return $_[0]->{att}->{$ID}; }

# methods used to add ids to elements that don't have one
BEGIN 
{ my $id_nb   = "0001";
  my $id_seed = "twig_id_";

  sub set_id_seed ## no critic (Subroutines::ProhibitNestedSubs);
    { $id_seed= $_[1]; $id_nb=1; }

  sub add_id ## no critic (Subroutines::ProhibitNestedSubs);
    { my $elt= shift; 
      if( defined $elt->{'att'}->{$ID})
        { return $elt->{'att'}->{$ID}; }
      else
        { my $id= $_[0] && ref( $_[0]) && isa( $_[0], 'CODE') ? $_[0]->( $elt) : $id_seed . $id_nb++; 
          $elt->set_id( $id);
          return $id;
        }
    }
}



# delete the id attribute and remove the element from the id list
sub del_id 
  { my $elt= shift; 
    if( ! exists $elt->{att}->{$ID}) { return $elt }; 
    my $id= $elt->{att}->{$ID};

    delete $elt->{att}->{$ID}; 

    my $t= shift || $elt->twig;
    unless( $t) { return $elt; }
    if( exists $t->{twig_id_list}->{$id}) { delete $t->{twig_id_list}->{$id}; }

    return $elt;
  }

# return the list of children
sub children
  { my $elt= shift;
    my @children;
    my $child= $elt->first_child( @_);
    while( $child) 
      { push @children, $child;
        $child= $child->next_sibling( @_);
      } 
    return @children;
  }

sub _children
  { my $elt= shift;
    my @children=();
    my $child= $elt->{first_child};
    while( $child) 
      { push @children, $child;
        $child= $child->{next_sibling};
      } 
    return @children;
  }

sub children_copy
  { my $elt= shift;
    my @children;
    my $child= $elt->first_child( @_);
    while( $child) 
      { push @children, $child->copy;
        $child= $child->next_sibling( @_);
      } 
    return @children;
  }


sub children_count
  { my $elt= shift;
    my $cond= shift;
    my $count=0;
    my $child= $elt->{first_child};
    while( $child)
      { $count++ if( $child->passes( $cond)); 
        $child= $child->{next_sibling};
      }
    return $count;
  }

sub children_text
  { my $elt= shift;
    return wantarray() ? map { $_->text} $elt->children( @_)
                       : join( '', map { $_->text} $elt->children( @_) )
                       ;
  }

sub children_trimmed_text
  { my $elt= shift;
    return wantarray() ? map { $_->trimmed_text} $elt->children( @_)
                       : join( '', map { $_->trimmed_text} $elt->children( @_) )
                       ;
  }

sub all_children_are
  { my( $parent, $cond)= @_;
    foreach my $child ($parent->_children)
      { return 0 unless( $child->passes( $cond)); }
    return $parent;
  }


sub ancestors
  { my( $elt, $cond)= @_;
    my @ancestors;
    while( $elt->{parent})
      { $elt= $elt->{parent};
        push @ancestors, $elt if( $elt->passes( $cond));
      }
    return @ancestors;
  }

sub ancestors_or_self
  { my( $elt, $cond)= @_;
    my @ancestors;
    while( $elt)
      { push @ancestors, $elt if( $elt->passes( $cond));
        $elt= $elt->{parent};
      }
    return @ancestors;
  }


sub _ancestors
  { my( $elt, $include_self)= @_;
    my @ancestors= $include_self ? ($elt) : ();
    while( $elt= $elt->{parent}) { push @ancestors, $elt; }
    return @ancestors;
  }


sub inherit_att
  { my $elt= shift;
    my $att= shift;
    my %tags= map { ($_, 1) } @_;

    do 
      { if(   (defined $elt->{'att'}->{$att})
           && ( !%tags || $tags{$XML::Twig::index2gi[$elt->{'gi'}]})
          )
          { return $elt->{'att'}->{$att}; }
      } while( $elt= $elt->{parent});
    return undef;
  }

sub _inherit_att_through_cut
  { my $elt= shift;
    my $att= shift;
    my %tags= map { ($_, 1) } @_;

    do 
      { if(   (defined $elt->{'att'}->{$att})
           && ( !%tags || $tags{$XML::Twig::index2gi[$elt->{'gi'}]})
          )
          { return $elt->{'att'}->{$att}; }
      } while( $elt= $elt->{parent} || $elt->former_parent);
    return undef;
  }


sub current_ns_prefixes
  { my $elt= shift;
    my %prefix;
    $prefix{''}=1 if( $elt->namespace( ''));
    while( $elt)
      { my @ns= grep { !m{^xml} } map { m{^([^:]+):} } ($XML::Twig::index2gi[$elt->{'gi'}], $elt->att_names);
        $prefix{$_}=1 foreach (@ns);
        $elt= $elt->{parent};
      }

    return (sort keys %prefix);
  }

# kinda counter-intuitive actually:
# the next element is found by looking for the next open tag after from the
# current one, which is the first child, if it exists, or the next sibling
# or the first next sibling of an ancestor
# optional arguments are: 
#   - $subtree_root: a reference to an element, when the next element is not 
#                    within $subtree_root anymore then next_elt returns undef
#   - $cond: a condition, next_elt returns the next element matching the condition
                 
sub next_elt
  { my $elt= shift;
    my $subtree_root= 0;
    $subtree_root= shift if( ref( $_[0]) && isa( $_[0], 'XML::Twig::Elt'));
    my $cond= shift;
    my $next_elt;

    my $ind;                                                              # optimization
    my $test_cond;
    if( $cond)                                                            # optimization
      { unless( defined( $ind= $XML::Twig::gi2index{$cond}) )             # optimization
          { $test_cond= ($cond_cache{$cond} ||= _install_cond( $cond)); } # optimization
      }                                                                   # optimization
    
    do
      { if( $next_elt= $elt->{first_child})
          { # simplest case: the elt has a child
          }
         elsif( $next_elt= $elt->{next_sibling}) 
          { # no child but a next sibling (just check we stay within the subtree)
          
            # case where elt is subtree_root, is empty and has a sibling
            return undef if( $subtree_root && ($elt == $subtree_root));
            
          }
        else
          { # case where the element has no child and no next sibling:
            # get the first next sibling of an ancestor, checking subtree_root 
          
            # case where elt is subtree_root, is empty and has no sibling
            return undef if( $subtree_root && ($elt == $subtree_root));
             
            $next_elt= $elt->{parent};

            until( $next_elt->{next_sibling})
              { return undef if( $subtree_root && ($subtree_root == $next_elt));
                $next_elt= $next_elt->{parent} || return undef;
              }
            return undef if( $subtree_root && ($subtree_root == $next_elt)); 
            $next_elt= $next_elt->{next_sibling};   
          }  
      $elt= $next_elt;                   # just in case we need to loop
    } until(    ! defined $elt 
             || ! defined $cond 
         || (defined $ind       && ($elt->{gi} eq $ind))   # optimization
         || (defined $test_cond && ($test_cond->( $elt)))
               );
    
      return $elt;
      }

# return the next_elt within the element
# just call next_elt with the element as first and second argument
sub first_descendant { return $_[0]->next_elt( @_); }

# get the last descendant, # then return the element found or call prev_elt with the condition
sub last_descendant
  { my( $elt, $cond)= @_;
    my $last_descendant= $elt->_last_descendant;
    if( !$cond || $last_descendant->matches( $cond))
      { return $last_descendant; }
    else
      { return $last_descendant->prev_elt( $elt, $cond); }
  }

# no argument allowed here, just go down the last_child recursively
sub _last_descendant
  { my $elt= shift;
    while( my $child= $elt->{last_child}) { $elt= $child; }
    return $elt;
  }

# counter-intuitive too:
# the previous element is found by looking
# for the first open tag backwards from the current one
# it's the last descendant of the previous sibling 
# if it exists, otherwise it's simply the parent
sub prev_elt
  { my $elt= shift;
    my $subtree_root= 0;
    if( defined $_[0] and (ref( $_[0]) && isa( $_[0], 'XML::Twig::Elt')))
      { $subtree_root= shift ;
        return undef if( $elt == $subtree_root);
      }
    my $cond= shift;
    # get prev elt
    my $prev_elt;
    do
      { return undef if( $elt == $subtree_root);
        if( $prev_elt= $elt->{prev_sibling})
          { while( $prev_elt->{last_child})
              { $prev_elt= $prev_elt->{last_child}; }
          }
        else
          { $prev_elt= $elt->{parent} || return undef; }
        $elt= $prev_elt;     # in case we need to loop 
      } until( $elt->passes( $cond));

    return $elt;
  }

sub _following_elt
  { my( $elt)= @_;
    while( $elt && !$elt->{next_sibling})
      { $elt= $elt->{parent}; }
    return $elt ? $elt->{next_sibling} : undef;
  }

sub following_elt
  { my( $elt, $cond)= @_;
    $elt= $elt->_following_elt || return undef;
    return $elt if( !$cond || $elt->matches( $cond));
    return $elt->next_elt( $cond);
  }

sub following_elts
  { my( $elt, $cond)= @_;
    if( !$cond) { undef $cond; }
    my $following= $elt->following_elt( $cond);
    if( $following)
      { my @followings= $following;
        while( $following= $following->next_elt( $cond))
          { push @followings, $following; }
        return( @followings);
      }
    else
      { return (); }
  }

sub _preceding_elt
  { my( $elt)= @_;
    while( $elt && !$elt->{prev_sibling})
      { $elt= $elt->{parent}; }
    return $elt ? $elt->{prev_sibling}->_last_descendant : undef;
  }

sub preceding_elt
  { my( $elt, $cond)= @_;
    $elt= $elt->_preceding_elt || return undef;
    return $elt if( !$cond || $elt->matches( $cond));
    return $elt->prev_elt( $cond);
  }

sub preceding_elts
  { my( $elt, $cond)= @_;
    if( !$cond) { undef $cond; }
    my $preceding= $elt->preceding_elt( $cond);
    if( $preceding)
      { my @precedings= $preceding;
        while( $preceding= $preceding->prev_elt( $cond))
          { push @precedings, $preceding; }
        return( @precedings);
      }
    else
      { return (); }
  }

# used in get_xpath
sub _self
  { my( $elt, $cond)= @_;
    return $cond ? $elt->matches( $cond) : $elt;
  }

sub next_n_elt
  { my $elt= shift;
    my $offset= shift || return undef;
    foreach (1..$offset)
      { $elt= $elt->next_elt( @_) || return undef; }
    return $elt;
  }

# checks whether $elt is included in $ancestor, returns 1 in that case
sub in
  { my ($elt, $ancestor)= @_;
    if( ref( $ancestor) && isa( $ancestor, 'XML::Twig::Elt'))
      { # element
        while( $elt= $elt->{parent}) { return $elt if( $elt ==  $ancestor); } 
      }
    else
      { # condition
        while( $elt= $elt->{parent}) { return $elt if( $elt->matches( $ancestor)); } 
      }
    return 0;           
  }

sub first_child_text  
  { my $elt= shift;
    my $dest=$elt->first_child(@_) or return '';
    return $dest->text;
  }

sub fields  
  { my $elt= shift;
    return map { $elt->field( $_) } @_;
  }

sub first_child_trimmed_text  
  { my $elt= shift;
    my $dest=$elt->first_child(@_) or return '';
    return $dest->trimmed_text;
  }
  
sub first_child_matches
  { my $elt= shift;
    my $dest= $elt->{first_child} or return undef;
    return $dest->passes( @_);
  }
  
sub last_child_text  
  { my $elt= shift;
    my $dest=$elt->last_child(@_) or return '';
    return $dest->text;
  }
  
sub last_child_trimmed_text  
  { my $elt= shift;
    my $dest=$elt->last_child(@_) or return '';
    return $dest->trimmed_text;
  }
  
sub last_child_matches
  { my $elt= shift;
    my $dest= $elt->{last_child} or return undef;
    return $dest->passes( @_);
  }
  
sub child_text
  { my $elt= shift;
    my $dest=$elt->child(@_) or return '';
    return $dest->text;
  }
  
sub child_trimmed_text
  { my $elt= shift;
    my $dest=$elt->child(@_) or return '';
    return $dest->trimmed_text;
  }
  
sub child_matches
  { my $elt= shift;
    my $nb= shift;
    my $dest= $elt->child( $nb) or return undef;
    return $dest->passes( @_);
  }

sub prev_sibling_text  
  { my $elt= shift;
    my $dest=$elt->_prev_sibling(@_) or return '';
    return $dest->text;
  }
  
sub prev_sibling_trimmed_text  
  { my $elt= shift;
    my $dest=$elt->_prev_sibling(@_) or return '';
    return $dest->trimmed_text;
  }
  
sub prev_sibling_matches
  { my $elt= shift;
    my $dest= $elt->{prev_sibling} or return undef;
    return $dest->passes( @_);
  }
  
sub next_sibling_text  
  { my $elt= shift;
    my $dest=$elt->next_sibling(@_) or return '';
    return $dest->text;
  }
  
sub next_sibling_trimmed_text  
  { my $elt= shift;
    my $dest=$elt->next_sibling(@_) or return '';
    return $dest->trimmed_text;
  }
  
sub next_sibling_matches
  { my $elt= shift;
    my $dest= $elt->{next_sibling} or return undef;
    return $dest->passes( @_);
  }
  
sub prev_elt_text  
  { my $elt= shift;
    my $dest=$elt->prev_elt(@_) or return '';
    return $dest->text;
  }
  
sub prev_elt_trimmed_text  
  { my $elt= shift;
    my $dest=$elt->prev_elt(@_) or return '';
    return $dest->trimmed_text;
  }
  
sub prev_elt_matches
  { my $elt= shift;
    my $dest= $elt->prev_elt or return undef;
    return $dest->passes( @_);
  }
  
sub next_elt_text  
  { my $elt= shift;
    my $dest=$elt->next_elt(@_) or return '';
    return $dest->text;
  }
  
sub next_elt_trimmed_text  
  { my $elt= shift;
    my $dest=$elt->next_elt(@_) or return '';
    return $dest->trimmed_text;
  }
  
sub next_elt_matches
  { my $elt= shift;
    my $dest= $elt->next_elt or return undef;
    return $dest->passes( @_);
  }
  
sub parent_text  
  { my $elt= shift;
    my $dest=$elt->parent(@_) or return '';
    return $dest->text;
  }
  
sub parent_trimmed_text  
  { my $elt= shift;
    my $dest=$elt->parent(@_) or return '';
    return $dest->trimmed_text;
  }
  
sub parent_matches
  { my $elt= shift;
    my $dest= $elt->{parent} or return undef;
    return $dest->passes( @_);
  }
 
sub is_first_child
  { my $elt= shift;
    my $parent= $elt->{parent} or return 0;
    my $first_child= $parent->first_child( @_) or return 0;
    return ($first_child == $elt) ? $elt : 0;
  }
 
sub is_last_child
  { my $elt= shift;
    my $parent= $elt->{parent} or return 0;
    my $last_child= $parent->last_child( @_) or return 0;
    return ($last_child == $elt) ? $elt : 0;
  }

# returns the depth level of the element
# if 2 parameter are used then counts the 2cd element name in the
# ancestors list
sub level
  { my( $elt, $cond)= @_;
    my $level=0;
    my $name=shift || '';
    while( $elt= $elt->{parent}) { $level++ if( !$cond || $elt->matches( $cond)); }
    return $level;           
  }

# checks whether $elt has an ancestor that satisfies $cond, returns the ancestor
sub in_context
  { my ($elt, $cond, $level)= @_;
    $level= -1 unless( $level) ;  # $level-- will never hit 0

    while( $level)
      { $elt= $elt->{parent} or return 0;
        if( $elt->matches( $cond)) { return $elt; }
        $level--;
      }
    return 0;
  }

sub _descendants
  { my( $subtree_root, $include_self)= @_;
    my @descendants= $include_self ? ($subtree_root) : ();

    my $elt= $subtree_root; 
    my $next_elt;   
 
    MAIN: while( 1)  
      { if( $next_elt= $elt->{first_child})
          { # simplest case: the elt has a child
          }
        elsif( $next_elt= $elt->{next_sibling}) 
          { # no child but a next sibling (just check we stay within the subtree)
          
            # case where elt is subtree_root, is empty and has a sibling
            last MAIN if( $elt == $subtree_root);
          }
        else
          { # case where the element has no child and no next sibling:
            # get the first next sibling of an ancestor, checking subtree_root 
                
            # case where elt is subtree_root, is empty and has no sibling
            last MAIN if( $elt == $subtree_root);
               
            # backtrack until we find a parent with a next sibling
            $next_elt= $elt->{parent} || last;
            until( $next_elt->{next_sibling})
              { last MAIN if( $subtree_root == $next_elt);
                $next_elt= $next_elt->{parent} || last MAIN;
              }
            last MAIN if( $subtree_root == $next_elt); 
            $next_elt= $next_elt->{next_sibling};   
          }  
        $elt= $next_elt || last MAIN;
        push @descendants, $elt;
      }
    return @descendants;
  }


sub descendants
  { my( $subtree_root, $cond)= @_;
    my @descendants=(); 
    my $elt= $subtree_root;
    
    # this branch is pure optimization for speed: if $cond is a gi replace it
    # by the index of the gi and loop here 
    # start optimization
    my $ind;
    if( !$cond || ( defined ( $ind= $XML::Twig::gi2index{$cond})) )
      {
        my $next_elt;

        while( 1)  
          { if( $next_elt= $elt->{first_child})
                { # simplest case: the elt has a child
                }
             elsif( $next_elt= $elt->{next_sibling}) 
              { # no child but a next sibling (just check we stay within the subtree)
           
                # case where elt is subtree_root, is empty and has a sibling
                last if( $subtree_root && ($elt == $subtree_root));
              }
            else
              { # case where the element has no child and no next sibling:
                # get the first next sibling of an ancestor, checking subtree_root 
                
                # case where elt is subtree_root, is empty and has no sibling
                last if( $subtree_root && ($elt == $subtree_root));
               
                # backtrack until we find a parent with a next sibling
                $next_elt= $elt->{parent} || last undef;
                until( $next_elt->{next_sibling})
                  { last if( $subtree_root && ($subtree_root == $next_elt));
                    $next_elt= $next_elt->{parent} || last;
                  }
                last if( $subtree_root && ($subtree_root == $next_elt)); 
                $next_elt= $next_elt->{next_sibling};   
              }  
            $elt= $next_elt || last;
            push @descendants, $elt if( !$cond || ($elt->{gi} eq $ind));
          }
      }
    else
    # end optimization
      { # branch for a complex condition: use the regular (slow but simple) way
        while( $elt= $elt->next_elt( $subtree_root, $cond))
          { push @descendants, $elt; }
      }
    return @descendants;
  }

 
sub descendants_or_self
  { my( $elt, $cond)= @_;
    my @descendants= $elt->passes( $cond) ? ($elt) : (); 
    push @descendants, $elt->descendants( $cond);
    return @descendants;
  }
  
sub sibling
  { my $elt= shift;
    my $nb= shift;
    if( $nb > 0)
      { foreach( 1..$nb)
          { $elt= $elt->next_sibling( @_) or return undef; }
      }
    elsif( $nb < 0)
      { foreach( 1..(-$nb))
          { $elt= $elt->prev_sibling( @_) or return undef; }
      }
    else # $nb == 0
      { return $elt->passes( $_[0]); }
    return $elt;
  }

sub sibling_text
  { my $elt= sibling( @_);
    return $elt ? $elt->text : undef;
  }


sub child
  { my $elt= shift;
    my $nb= shift;
    if( $nb >= 0)
      { $elt= $elt->first_child( @_) or return undef;
        foreach( 1..$nb)
          { $elt= $elt->next_sibling( @_) or return undef; }
      }
    else
      { $elt= $elt->last_child( @_) or return undef;
        foreach( 2..(-$nb))
          { $elt= $elt->prev_sibling( @_) or return undef; }
      }
    return $elt;
  }

sub prev_siblings
  { my $elt= shift;
    my @siblings=();
    while( $elt= $elt->prev_sibling( @_))
      { unshift @siblings, $elt; }
    return @siblings;
  }

sub siblings
  { my $elt= shift;
    return grep { $_ ne $elt } $elt->{parent}->children( @_);
  }

sub pos
  { my $elt= shift;
    return 0 if ($_[0] && !$elt->matches( @_));
    my $pos=1;
    $pos++ while( $elt= $elt->prev_sibling( @_));
    return $pos;
  }


sub next_siblings
  { my $elt= shift;
    my @siblings=();
    while( $elt= $elt->next_sibling( @_))
      { push @siblings, $elt; }
    return @siblings;
  }


# used by get_xpath: parses the xpath expression and generates a sub that performs the
# search
{ my %axis2method;
  BEGIN { %axis2method= ( child               => 'children',
                          descendant          => 'descendants',
                         'descendant-or-self' => 'descendants_or_self',
                          parent              => 'parent_is',
                          ancestor            => 'ancestors',
                         'ancestor-or-self'   => 'ancestors_or_self',
                         'following-sibling'  => 'next_siblings',
                         'preceding-sibling'  => 'prev_siblings',
                          following           => 'following_elts',
                          preceding           => 'preceding_elts',
                          self                => '_self',
                        );
        }

  sub _install_xpath
    { my( $xpath_exp, $type)= @_;
      my $original_exp= $xpath_exp;
      my $sub= 'my $elt= shift; my @results;';
      
      # grab the root if expression starts with a /
      if( $xpath_exp=~ s{^/}{})
        { $sub .= '@results= ($elt->twig) || croak "cannot use an XPath query starting with a / on a node not attached to a whole twig";'; }
      elsif( $xpath_exp=~ s{^\./}{})
        { $sub .= '@results= ($elt);'; }
      else
        { $sub .= '@results= ($elt);'; }
  
 
     #warn "xpath_exp= '$xpath_exp'\n";

      while( $xpath_exp &&
             $xpath_exp=~s{^\s*(/?)                            
                            # the xxx=~/regexp/ is a pain as it includes /  
                            (\s*(?:(?:($REG_AXIS)::)?(\*|$REG_NAME|\.\.|\.)\s*)?($REG_PREDICATE_ALT*)
                            )
                            (/|$)}{}xo)
  
        { my( $wildcard, $sub_exp, $axis, $gi, $predicates)= ($1, $2, $3, $4, $5);
          
          # grab a parent
          if( $sub_exp eq '..')
            { _croak_and_doublecheck_xpath( $original_exp, "error in xpath expression $original_exp") if( $wildcard);
              $sub .= '@results= map { $_->{parent}} @results;';
            }
          # test the element itself
          elsif( $sub_exp=~ m{^\.(.*)$}s)
            { $sub .= "\@results= grep { \$_->matches( q{$1}) } \@results;" }
              # grab children
          else       
            { 
              if( !$axis)             
                { $axis= $wildcard ? 'descendant' : 'child'; }
              if( !$gi or $gi eq '*') { $gi=''; }
              my $function;
  
              # "special" predicates, that return just one element
              if( $predicates && ($predicates =~ m{^\s*\[\s*((-\s*)?\d+)\s*\]\s*$}))
                { # [<nb>]
                  my $offset= $1;
                  $offset-- if( $offset > 0);
                  $function=  $axis eq 'descendant' ? "next_n_elt( $offset, '$gi')" 
                           :  $axis eq 'child'      ? "child( $offset, '$gi')"
                           :                          _croak_and_doublecheck_xpath( $original_exp, "error [$1] not supported along axis '$axis'")
                           ;
                  $sub .= "\@results= grep { \$_ } map { \$_->$function } \@results;"
                }
              elsif( $predicates && ($predicates =~ m{^\s*\[\s*last\s*\(\s*\)\s*\]\s*$}) )
                { # last()
                  _croak_and_doublecheck_xpath( $original_exp, "error in xpath expression $original_exp, usage of // and last() not supported") if( $wildcard);
                   $sub .= "\@results= map { \$_->last_child( '$gi') } \@results;";
                }
              else
                { # follow the axis
                  #warn "axis: '$axis' - method: '$axis2method{$axis}' - gi: '$gi'\n";

                  my $follow_axis= " \$_->$axis2method{$axis}( '$gi')";
                  my $step= $follow_axis;
                  
                  # now filter using the predicate
                  while( $predicates=~ s{^\s*($REG_PREDICATE_ALT)\s*}{}o)
                    { my $pred= $1;
                      $pred=~ s{^\s*\[\s*}{};
                      $pred=~ s{\s*\]\s*$}{};
                      my $test="";
                      my $pos;
                      if( $pred=~ m{^(-?\s*\d+)$})
                        { my $pos= $1;
                          if( $step=~ m{^\s*grep(.*) (\$_->\w+\(\s*'[^']*'\s*\))})
                            { $step= "XML::Twig::_first_n $1 $pos, $2"; }
                          else
                            { if( $pos > 0) { $pos--; }
                              $step= "($step)[$pos]"; 
                            }
                          #warn "number predicate '$pos' - generated step '$step'\n";
                        }
                      else
                        { my $syntax_error=0;
                          do
                            { if( $pred =~ s{^string\(\s*\)\s*=\s*($REG_STRING)\s*}{}o)  # string()="string" pred
                                { $test .= "\$_->text eq $1"; }
                              elsif( $pred =~ s{^string\(\s*\)\s*!=\s*($REG_STRING)\s*}{}o)  # string()!="string" pred
                                { $test .= "\$_->text ne $1"; }
                              if( $pred =~ s{^string\(\s*\)\s*=\s*($REG_NUMBER)\s*}{}o)  # string()=<number> pred
                                { $test .= "\$_->text eq $1"; }
                              elsif( $pred =~ s{^string\(\s*\)\s*!=\s*($REG_NUMBER)\s*}{}o)  # string()!=<number> pred
                                { $test .= "\$_->text ne $1"; }
                              elsif( $pred =~ s{^string\(\s*\)\s*(>|<|>=|<=)\s*($REG_NUMBER)\s*}{}o)  # string()!=<number> pred
                                { $test .= "\$_->text $1 $2"; }

                             elsif( $pred =~ s{^string\(\s*\)\s*($REG_MATCH)\s*($REG_REGEXP)\s*}{}o)  # string()=~/regex/ pred
                                { my( $match, $regexp)= ($1, $2);
                                  $test .= "\$_->text $match $regexp"; 
                                }
                              elsif( $pred =~ s{^string\(\s*\)\s*}{}o)  # string() pred
                                { $test .= "\$_->text"; }
                             elsif( $pred=~ s{^@($REG_NAME)\s*($REG_OP)\s*($REG_STRING|$REG_NUMBER)}{}o)  # @att="val" pred
                                { my( $att, $oper, $val)= ($1, _op( $2), $3);
                                  $test .= qq{((defined \$_->{'att'}->{"$att"})  && (\$_->{'att'}->{"$att"} $oper $val))};
                                }
                             elsif( $pred =~ s{^@($REG_NAME)\s*($REG_MATCH)\s*($REG_REGEXP)\s*}{}o)  # @att=~/regex/ pred XXX
                                { my( $att, $match, $regexp)= ($1, $2, $3);
                                  $test .= qq{((defined \$_->{'att'}->{"$att"})  && (\$_->{'att'}->{"$att"} $match $regexp))};; 
                                }
                             elsif( $pred=~ s{^@($REG_NAME)\s*}{}o)                      # @att pred
                                { $test .= qq{(defined \$_->{'att'}->{"$1"})}; }
                             elsif( $pred=~ s{^\s*(?:not|!)\s*@($REG_NAME)\s*}{}o)       # not @att pred
                                { $test .= qq{((\$_->is_elt) && (not defined \$_->{'att'}->{"$1"}))}; }
                              elsif( $pred=~ s{^\s*([()])}{})                            # ( or ) (just add to the test)
                                { $test .= qq{$1};           }
                              elsif( $pred=~ s{^\s*(and|or)\s*}{})
                                { $test .= lc " $1 "; }
                              else
                                { $syntax_error=1; }
                             
                             } while( !$syntax_error && $pred);
                           _croak_and_doublecheck_xpath( $original_exp, "error in xpath expression $original_exp at $pred") if( $pred);
                           $step= " grep { $test } $step ";
                        }
                    }
                  #warn "step: '$step'";
                  $sub .= "\@results= grep { \$_ } map { $step } \@results;"; 
                }
            }
        }
  
      if( $xpath_exp)
        { _croak_and_doublecheck_xpath( $original_exp, "error in xpath expression $original_exp around $xpath_exp"); }
        
      $sub .= q{return XML::Twig::_unique_elts( @results); };
      #warn "generated: '$sub'\n";
      my $s= eval "sub { $NO_WARNINGS; $sub }";
      if( $@) 
        { _croak_and_doublecheck_xpath( $original_exp, "error in xpath expression $original_exp ($@);") }
      return( $s); 
    }
}

sub _croak_and_doublecheck_xpath
  { my $xpath_expression= shift;
    my $mess= join( "\n", @_);
    if( $XML::Twig::XPath::VERSION || 0) 
      { my $check_twig= XML::Twig::XPath->new;
        if( eval { $check_twig->{twig_xp}->_parse( $xpath_expression) })
          { $mess .= "\nthe expression is a valid XPath statement, and you are using XML::Twig::XPath, but"
                   . "\nyou are using either 'find_nodes' or 'get_xpath' where the method you likely wanted"
                   . "\nto use is 'findnodes', which is the only one that uses the full XPath engine\n";
          }
      }
    croak $mess;
  }
    
    
           
{ # extremely elaborate caching mechanism
  my %xpath; # xpath_expression => subroutine_code;  
  sub get_xpath
    { my( $elt, $xpath_exp, $offset)= @_;
      my $sub= ($xpath{$xpath_exp} ||= _install_xpath( $xpath_exp));
      return $sub->( $elt) unless( defined $offset); 
      my @res= $sub->( $elt);
      return $res[$offset];
    }
}


sub findvalues
  { my $elt= shift;
    return map { $_->text } $elt->get_xpath( @_);
  }

sub findvalue
  { my $elt= shift;
    return join '', map { $_->text } $elt->get_xpath( @_);
  }


# XML::XPath compatibility
sub getElementById     { return $_[0]->twig->elt_id( $_[1]); }
sub getChildNodes      { my @children= do { my $elt= $_[0]; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; }; return wantarray ? @children : \@children; }

sub _flushed     { return $_[0]->{flushed}; }
sub _set_flushed { $_[0]->{flushed}=1;      }
sub _del_flushed { delete $_[0]->{flushed}; }

sub cut
  { my $elt= shift;
    my( $parent, $prev_sibling, $next_sibling, $last_elt);

    # you can't cut the root, sorry
    unless( $parent= $elt->{parent}) { return; }

    # save the old links, that'll make it easier for some loops
    foreach my $link ( qw(parent prev_sibling next_sibling) )
      { $elt->{former}->{$link}= $elt->{$link};
         if( $XML::Twig::weakrefs) { weaken( $elt->{former}->{$link}); }
      }

    # it we cut the current element then its parent becomes the current elt
    if( $elt->{twig_current})
      { my $twig_current= $elt->{parent};
        my $t= $elt->twig;
        $t->{twig_current}= $twig_current;
        $twig_current->{'twig_current'}=1;
        delete $elt->{'twig_current'};
      }

    if( $parent->{first_child} == $elt)
      { $parent->{first_child}=  $elt->{next_sibling};
        # cutting can make the parent empty
        if( ! $parent->{first_child}) { $parent->{empty}=  1; }
      }

    if( $parent->{last_child} == $elt)
      {  $parent->{empty}=0; $parent->{last_child}=$elt->{prev_sibling}; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ;
      }

    if( $prev_sibling= $elt->{prev_sibling})
      { $prev_sibling->{next_sibling}=  $elt->{next_sibling}; }
    if( $next_sibling= $elt->{next_sibling})
      { $next_sibling->{prev_sibling}=$elt->{prev_sibling}; if( $XML::Twig::weakrefs) { weaken( $next_sibling->{prev_sibling});} ; }


    $elt->{parent}=undef; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
    $elt->{prev_sibling}=undef; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;
    $elt->{next_sibling}=  undef;

    # merge 2 (now) consecutive text nodes if they are of the same type 
    # (type can be PCDATA or CDATA)
    if( $prev_sibling && $next_sibling && $prev_sibling->is_text && ( $XML::Twig::index2gi[$prev_sibling->{'gi'}] eq $XML::Twig::index2gi[$next_sibling->{'gi'}]))
      { $prev_sibling->merge_text( $next_sibling); }

    return $elt;
  }


sub former_next_sibling { return $_[0]->{former}->{next_sibling}; }
sub former_prev_sibling { return $_[0]->{former}->{prev_sibling}; }
sub former_parent       { return $_[0]->{former}->{parent};       }

sub cut_children
  { my( $elt, $exp)= @_;
    my @children= $elt->children( $exp);
    foreach (@children) { $_->cut; }
    if( ! $elt->has_children) { $elt->{empty}=  1; }
    return @children;
  }

sub cut_descendants
  { my( $elt, $exp)= @_;
    my @descendants= $elt->descendants( $exp);
    foreach ($elt->descendants( $exp)) { $_->cut; }
    if( ! $elt->has_children) { $elt->{empty}=  1; }
    return @descendants;
  }



sub erase
  { my $elt= shift;
    #you cannot erase the current element
    if( $elt->{twig_current})
      { croak "trying to erase an element before it has been completely parsed"; }
    unless( $elt->{parent})
      { # trying to erase the root (of a twig or of a cut/new element)
        my @children= do { my $elt= $elt; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; };
        unless( @children == 1)
          { croak "can only erase an element with no parent if it has a single child"; }
        $elt->_move_extra_data_after_erase;
        my $child= shift @children;
        $child->{parent}=undef; if( $XML::Twig::weakrefs) { weaken( $child->{parent});} ;
        my $twig= $elt->twig;
        $twig->set_root( $child);
      }
    else     
      { # normal case
        $elt->_move_extra_data_after_erase;
        my @children= do { my $elt= $elt; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; };
        if( @children)
          { # elt has children, move them up

            my $first_child= $elt->{first_child};
            my $prev_sibling=$elt->{prev_sibling};
            if( $prev_sibling)
              { # connect first child to previous sibling
                $first_child->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $first_child->{prev_sibling});} ;      
                $prev_sibling->{next_sibling}=  $first_child; 
              }
            else
              { # elt was the first child
                $elt->{parent}->set_first_child( $first_child);
              }

            my $last_child= $elt->{last_child};
            my $next_sibling= $elt->{next_sibling};
            if( $next_sibling)
              { # connect last child to next sibling
                $last_child->{next_sibling}=  $next_sibling;      
                $next_sibling->{prev_sibling}=$last_child; if( $XML::Twig::weakrefs) { weaken( $next_sibling->{prev_sibling});} ; 
              }
            else
              { # elt was the last child
                $elt->{parent}->set_last_child( $last_child);
              }
            # update parent for all siblings
            foreach my $child (@children)
              { $child->{parent}=$elt->{parent}; if( $XML::Twig::weakrefs) { weaken( $child->{parent});} ; }

            # merge consecutive text elements if need be
            if( $prev_sibling && $prev_sibling->is_text && ($XML::Twig::index2gi[$first_child->{'gi'}] eq $XML::Twig::index2gi[$prev_sibling->{'gi'}]) )
              { $prev_sibling->merge_text( $first_child); }
            if( $next_sibling && $next_sibling->is_text && ($XML::Twig::index2gi[$last_child->{'gi'}] eq $XML::Twig::index2gi[$next_sibling->{'gi'}]) )
              { $last_child->merge_text( $next_sibling); }

            # if parsing and have now a PCDATA text, mark so we can normalize later on if need be
            if( $elt->{parent}->{twig_current} && $elt->{last_child}->is_text) {  $elt->{parent}->{twig_to_be_normalized}=1; }

            # elt is not referenced any more, so it will be DESTROYed
            # so we'd better break the links to its children
            undef $elt->{first_child};
            undef $elt->{last_child};
            undef $elt->{parent};
            undef $elt->{prev_sibling};
            undef $elt->{next_sibling};
 
          }
          { # elt had no child, delete it
             $elt->delete;
          }
              
      }
    return $elt;

  }

sub _move_extra_data_after_erase
  { my( $elt)= @_;
    # extra_data
    if( my $extra_data= $elt->{extra_data}) 
      { my $target= $elt->{first_child} || $elt->{next_sibling};
        if( $target)
          {
            if( $target->is( $ELT))
              { $target->set_extra_data( $extra_data . ($target->extra_data || '')); }
            elsif( $target->is( $TEXT))
              { $target->_unshift_extra_data_in_pcdata( $extra_data, 0); }  # TO CHECK
          }
        else
          { my $parent= $elt->{parent}; # always exists or the erase cannot be performed
            $parent->_prefix_extra_data_before_end_tag( $extra_data); 
          }
      }
      
     # extra_data_before_end_tag
    if( my $extra_data= $elt->{extra_data_before_end_tag}) 
      { if( my $target= $elt->{next_sibling})
          { if( $target->is( $ELT))
              { $target->set_extra_data( $extra_data . ($target->extra_data || '')); }
            elsif( $target->is( $TEXT))
              { 
                $target->_unshift_extra_data_in_pcdata( $extra_data, 0);
             }
          }
        elsif( my $parent= $elt->{parent})
          { $parent->_prefix_extra_data_before_end_tag( $extra_data); }
       }

    return $elt;

  }
BEGIN
  { my %method= ( before      => \&paste_before,
                  after       => \&paste_after,
                  first_child => \&paste_first_child,
                  last_child  => \&paste_last_child,
                  within      => \&paste_within,
        );
    
    # paste elt somewhere around ref
    # pos can be first_child (default), last_child, before, after or within
    sub paste ## no critic (Subroutines::ProhibitNestedSubs);
      { my $elt= shift;
        if( $elt->{parent}) 
          { croak "cannot paste an element that belongs to a tree"; }
        my $pos;
        my $ref;
        if( ref $_[0]) 
          { $pos= 'first_child'; 
            croak "wrong argument order in paste, should be $_[1] first" if($_[1]); 
          }
        else
          { $pos= shift; }

        if( my $method= $method{$pos})
          {
            unless( ref( $_[0]) && isa( $_[0], 'XML::Twig::Elt'))
              { if( ! defined( $_[0]))
                  { croak "missing target in paste"; }
                elsif( ! ref( $_[0]))
                  { croak "wrong target type in paste (not a reference), should be XML::Twig::Elt or a subclass"; }
                else
                  { my $ref= ref $_[0];
                    croak "wrong target type in paste: '$ref', should be XML::Twig::Elt or a subclass";
                  }
              }
            $ref= $_[0];
            # check here so error message lists the caller file/line
            if( !$ref->{parent} && ($pos=~ m{^(before|after)$}) && !(exists $elt->{'target'}) && !(exists $elt->{'comment'})) 
              { croak "cannot paste $1 root"; }
            $elt->$method( @_); 
          }
        else
          { croak "tried to paste in wrong position '$pos', allowed positions " . 
              " are 'first_child', 'last_child', 'before', 'after' and "    .
              "'within'";
          }
        if( (my $ids= $elt->{twig_id_list}) && (my $t= $ref->twig) )
          { $t->{twig_id_list}||={};
            foreach my $id (keys %$ids)
              { $t->{twig_id_list}->{$id}= $ids->{$id}; 
                if( $XML::Twig::weakrefs) { weaken( $t->{twig_id_list}->{$id}); }
              }
          }
        return $elt;
      }
  

    sub paste_before
      { my( $elt, $ref)= @_;
        my( $parent, $prev_sibling, $next_sibling );
        
        # trying to paste before an orphan (root or detached wlt)
        unless( $ref->{parent}) 
          { if( my $t= $ref->twig)
              { if( (exists $elt->{'comment'}) || (exists $elt->{'target'})) # we can still do this
                  { $t->_add_cpi_outside_of_root( leading_cpi => $elt); return; }
                else
                  { croak "cannot paste before root"; }
              }
            else
              { croak "cannot paste before an orphan element"; }
          }
        $parent= $ref->{parent};
        $prev_sibling= $ref->{prev_sibling};
        $next_sibling= $ref;

        $elt->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
        if( $parent->{first_child} == $ref) { $parent->{first_child}=  $elt; }

        if( $prev_sibling) { $prev_sibling->{next_sibling}=  $elt; }
        $elt->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;

        $next_sibling->{prev_sibling}=$elt; if( $XML::Twig::weakrefs) { weaken( $next_sibling->{prev_sibling});} ;
        $elt->{next_sibling}=  $ref;
        return $elt;
      }
     
     sub paste_after
      { my( $elt, $ref)= @_;
        my( $parent, $prev_sibling, $next_sibling );

        # trying to paste after an orphan (root or detached wlt)
        unless( $ref->{parent}) 
            { if( my $t= $ref->twig)
                { if( (exists $elt->{'comment'}) || (exists $elt->{'target'})) # we can still do this
                    { $t->_add_cpi_outside_of_root( trailing_cpi => $elt); return; }
                  else
                    { croak "cannot paste after root"; }
                }
              else
                { croak "cannot paste after an orphan element"; }
            }
        $parent= $ref->{parent};
        $prev_sibling= $ref;
        $next_sibling= $ref->{next_sibling};

        $elt->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
        if( $parent->{last_child}== $ref) {  $parent->{empty}=0; $parent->{last_child}=$elt; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ; }

        $prev_sibling->{next_sibling}=  $elt;
        $elt->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;

        if( $next_sibling) { $next_sibling->{prev_sibling}=$elt; if( $XML::Twig::weakrefs) { weaken( $next_sibling->{prev_sibling});} ; }
        $elt->{next_sibling}=  $next_sibling;
        return $elt;

      }

    sub paste_first_child
      { my( $elt, $ref)= @_;
        my( $parent, $prev_sibling, $next_sibling );
        $parent= $ref;
        $next_sibling= $ref->{first_child};

        $elt->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
        $parent->{first_child}=  $elt;
        unless( $parent->{last_child}) {  $parent->{empty}=0; $parent->{last_child}=$elt; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ; }

        $elt->{prev_sibling}=undef; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;

        if( $next_sibling) { $next_sibling->{prev_sibling}=$elt; if( $XML::Twig::weakrefs) { weaken( $next_sibling->{prev_sibling});} ; }
        $elt->{next_sibling}=  $next_sibling;
        return $elt;
      }
      
    sub paste_last_child
      { my( $elt, $ref)= @_;
        my( $parent, $prev_sibling, $next_sibling );
        $parent= $ref;
        $prev_sibling= $ref->{last_child};

        $elt->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
         $parent->{empty}=0; $parent->{last_child}=$elt; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ;
        unless( $parent->{first_child}) { $parent->{first_child}=  $elt; }

        $elt->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;
        if( $prev_sibling) { $prev_sibling->{next_sibling}=  $elt; }

        $elt->{next_sibling}=  undef;
        return $elt;
      }

    sub paste_within
      { my( $elt, $ref, $offset)= @_;
        my $text= $ref->is_text ? $ref : $ref->next_elt( $TEXT, $ref);
        my $new= $text->split_at( $offset);
        $elt->paste_before( $new);
        return $elt;
      }
  }

# load an element into a structure similar to XML::Simple's
sub simplify
  { my $elt= shift;

    # normalize option names
    my %options= @_;
    %options= map { my ($key, $val)= ($_, $options{$_});
                       $key=~ s{(\w)([A-Z])}{$1_\L$2}g;
                       $key => $val
                     } keys %options;

    # check options
    my @allowed_options= qw( keyattr forcearray noattr content_key
                             var var_regexp variables var_attr 
                             group_tags forcecontent
                             normalise_space normalize_space
                   );
    my %allowed_options= map { $_ => 1 } @allowed_options;
    foreach my $option (keys %options)
      { carp "invalid option $option\n" unless( $allowed_options{$option}); }

    $options{normalise_space} ||= $options{normalize_space} || 0;
    
    $options{content_key} ||= 'content';
    if( $options{content_key}=~ m{^-})
      { # need to remove the - and to activate extra folding
        $options{content_key}=~ s{^-}{};
        $options{extra_folding}= 1;
      }
    else
      { $options{extra_folding}= 0; }
   
    $options{forcearray} ||=0; 
    if( isa( $options{forcearray}, 'ARRAY'))
      { my %forcearray_tags= map { $_ => 1 } @{$options{forcearray}};
        $options{forcearray_tags}= \%forcearray_tags;
        $options{forcearray}= 0;
      }

    $options{keyattr}     ||= ['name', 'key', 'id'];
    if( ref $options{keyattr} eq 'ARRAY')
      { foreach my $keyattr (@{$options{keyattr}})
          { my( $prefix, $att)= ($keyattr=~ m{^([+-])?(.*)});
            $prefix ||= '';
            $options{key_for_all}->{$att}= 1;
            $options{remove_key_for_all}->{$att}=1 unless( $prefix eq '+');
            $options{prefix_key_for_all}->{$att}=1 if( $prefix eq '-');
          }
      }
    elsif( ref $options{keyattr} eq 'HASH')
      { while( my( $elt, $keyattr)= each %{$options{keyattr}})
         { my( $prefix, $att)= ($keyattr=~ m{^([+-])?(.*)});
           $prefix ||='';
           $options{key_for_elt}->{$elt}= $att;
           $options{remove_key_for_elt}->{"$elt#$att"}=1 unless( $prefix);
           $options{prefix_key_for_elt}->{"$elt#$att"}=1 if( $prefix eq '-');
         }
      }
       

    $options{var}||= $options{var_attr}; # for compat with XML::Simple
    if( $options{var}) { $options{var_values}= {}; }
    else               { $options{var}='';         }

    if( $options{variables}) 
      { $options{var}||= 1;
        $options{var_values}= $options{variables};
      }

    if( $options{var_regexp} and !$options{var})
      { warn "var option not used, var_regexp option ignored\n"; }
    $options{var_regexp} ||= '\$\{?(\w+)\}?';
      
    $elt->_simplify( \%options);
 
 }

sub _simplify
  { my( $elt, $options)= @_;

    my $data;

    my $gi= $XML::Twig::index2gi[$elt->{'gi'}];
    my @children= do { my $elt= $elt; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; };
    my %atts= $options->{noattr} || !$elt->{att} ? () : %{$elt->{att}};
    my $nb_atts= keys %atts;
    my $nb_children= $elt->children_count + $nb_atts;

    my %nb_children;
    foreach (@children)   { $nb_children{$_->tag}++; }
    foreach (keys %atts)  { $nb_children{$_}++;      }

    my $arrays; # tag => array where elements are stored


    # store children
    foreach my $child (@children)
      { if( $child->is_text)
          { # generate with a content key
            my $text= $elt->_text_with_vars( $options);
            if( $options->{normalise_space} >= 2) { $text= _normalize_space( $text); }
            if(    $options->{force_content}
                || $nb_atts 
                || (scalar @children > 1)
              )
              { $data->{$options->{content_key}}= $text; }
            else
              { $data= $text; }
          }
        else
          { # element with sub-elements
            my $child_gi= $XML::Twig::index2gi[$child->{'gi'}];

            my $child_data= $child->_simplify( $options);

            # first see if we need to simplify further the child data
            # simplify because of grouped tags
            if( my $grouped_tag= $options->{group_tags}->{$child_gi})
              { # check that the child data is a hash with a single field
                unless(    (ref( $child_data) eq 'HASH')
                        && (keys %$child_data == 1)
                        && defined ( my $grouped_child_data= $child_data->{$grouped_tag})
                      )
                  { croak "error in grouped tag $child_gi"; }
                else
                  { $child_data=  $grouped_child_data; }
              }
            # simplify because of extra folding
            if( $options->{extra_folding})
              { if(    (ref( $child_data) eq 'HASH')
                    && (keys %$child_data == 1)
                    && defined( my $content= $child_data->{$options->{content_key}})
                  )
                  { $child_data= $content; }
              }

            if( my $keyatt= $child->_key_attr( $options))
              { # simplify element with key
                my $key= $child->{'att'}->{$keyatt};
                if( $options->{normalise_space} >= 1) { $key= _normalize_space( $key); }
                $data->{$child_gi}->{$key}= $child_data;
              }
            elsif(      $options->{forcearray}
                   ||   $options->{forcearray_tags}->{$child_gi}
                   || ( $nb_children{$child_gi} > 1)
                 )
              { # simplify element to store in an array
                $data->{$child_gi} ||= [];
                push @{$data->{$child_gi}}, $child_data;
              }
            else
              { # simplify element to store as a hash field
                $data->{$child_gi}= $child_data;
              }
          }
    }

    # store atts
    # TODO: deal with att that already have an element by that name
    foreach my $att (keys %atts)
      { # do not store if the att is a key that needs to be removed
        if(    $options->{remove_key_for_all}->{$att}
            || $options->{remove_key_for_elt}->{"$gi#$att"}
          )
          { next; }

        my $att_text= $options->{var} ?  _replace_vars_in_text( $atts{$att}, $options) : $atts{$att} ;
        if( $options->{normalise_space} >= 2) { $att_text= _normalize_space( $att_text); }
        
        if(    $options->{prefix_key_for_all}->{$att}
            || $options->{prefix_key_for_elt}->{"$gi#$att"}
          )
          { # prefix the att
            $data->{"-$att"}= $att_text;
          } 
        else
          { # normal case
            $data->{$att}= $att_text; 
          }
      }
    
    return $data;
  }

sub _key_attr
  { my( $elt, $options)=@_;
    return if( $options->{noattr});
    if( $options->{key_for_all})
      { foreach my $att ($elt->att_names)
          { if( $options->{key_for_all}->{$att})
              { return $att; }
          }
      }
    elsif( $options->{key_for_elt})
      { if( my $key_for_elt= $options->{key_for_elt}->{$XML::Twig::index2gi[$elt->{'gi'}]} )
          { return $key_for_elt if( defined( $elt->{'att'}->{$key_for_elt})); }
      }
    return;
  }

sub _text_with_vars
  { my( $elt, $options)= @_;
    my $text;
    if( $options->{var}) 
      { $text= _replace_vars_in_text( $elt->text, $options); 
        $elt->_store_var( $options);
      }
     else
      { $text= $elt->text; }
    return $text;
  }


sub _normalize_space
  { my $text= shift;
    $text=~ s{\s+}{ }sg;
    $text=~ s{^\s}{};
    $text=~ s{\s$}{};
    return $text;
  }


sub att_nb
  { return 0 unless( my $atts= $_[0]->{att});
    return scalar keys %$atts;
  }
    
sub has_no_atts
  { return 1 unless( my $atts= $_[0]->{att});
    return scalar keys %$atts ? 0 : 1;
  }
    
sub _replace_vars_in_text
  { my( $text, $options)= @_;

    $text=~ s{($options->{var_regexp})}
             { if( defined( my $value= $options->{var_values}->{$2}))
                 { $value }
               else
                 { warn "unknown variable $2\n";
                   $1
                 }
             }gex;
    return $text;
  }

sub _store_var
  { my( $elt, $options)= @_;
    if( defined (my $var_name= $elt->{'att'}->{$options->{var}}))
       { $options->{var_values}->{$var_name}= $elt->text; 
       }
  }


# split a text element at a given offset
sub split_at
  { my( $elt, $offset)= @_;
    my $text_elt= $elt->is_text ? $elt : $elt->first_child( $TEXT) || return '';
    my $string= $text_elt->text; 
    my $left_string= substr( $string, 0, $offset);
    my $right_string= substr( $string, $offset);
    $text_elt->{pcdata}= (delete $text_elt->{empty} || 1) &&  $left_string;
    my $new_elt= $elt->new( $XML::Twig::index2gi[$elt->{'gi'}], $right_string);
    $new_elt->paste( after => $elt);
    return $new_elt;
  }

    
# split an element or its text descendants into several, in place
# all elements (new and untouched) are returned
sub split    
  { my $elt= shift;
    my @text_chunks;
    my @result;
    if( $elt->is_text) { @text_chunks= ($elt); }
    else               { @text_chunks= $elt->descendants( $TEXT); }
    foreach my $text_chunk (@text_chunks)
      { push @result, $text_chunk->_split( 1, @_); }
    return @result;
  }

# split an element or its text descendants into several, in place
# created elements (those which match the regexp) are returned
sub mark
  { my $elt= shift;
    my @text_chunks;
    my @result;
    if( $elt->is_text) { @text_chunks= ($elt); }
    else               { @text_chunks= $elt->descendants( $TEXT); }
    foreach my $text_chunk (@text_chunks)
      { push @result, $text_chunk->_split( 0, @_); }
    return @result;
  }

# split a single text element
# return_all defines what is returned: if it is true 
# only returns the elements created by matches in the split regexp
# otherwise all elements (new and untouched) are returned


{ 
 
  sub _split
    { my $elt= shift;
      my $return_all= shift;
      my $regexp= shift;
      my @tags;

      while( @_)
        { my $tag= shift();
          if( ref $_[0]) 
            { push @tags, { tag => $tag, atts => shift }; }
          else
            { push @tags, { tag => $tag }; }
        }

      unless( @tags) { @tags= { tag => $elt->{parent}->gi }; }
          
      my @result;                                 # the returned list of elements
      my $text= $elt->text;
      my $gi= $XML::Twig::index2gi[$elt->{'gi'}];
  
      # 2 uses: if split matches then the first substring reuses $elt
      #         once a split has occured then the last match needs to be put in
      #         a new element      
      my $previous_match= 0;

      while( my( $pre_match, @matches)= $text=~ /^(.*?)$regexp(.*)$/gcs)
        { $text= pop @matches;
          if( $previous_match)
            { # match, not the first one, create a new text ($gi) element
              _utf8_ify( $pre_match) if( $] < 5.010);
              $elt= $elt->insert_new_elt( after => $gi, $pre_match);
              push @result, $elt if( $return_all);
            }
          else
            { # first match in $elt, re-use $elt for the first sub-string
              _utf8_ify( $pre_match) if( $] < 5.010);
              $elt->set_text( $pre_match);
              $previous_match++;                # store the fact that there was a match
              push @result, $elt if( $return_all);
            }

          # now deal with matches captured in the regexp
          if( @matches)
            { # match, with capture
              my $i=0;
              foreach my $match (@matches)
                { # create new element, text is the match
                  _utf8_ify( $match) if( $] < 5.010);
                  my $tag  = _repl_match( $tags[$i]->{tag}, @matches) || '#PCDATA';
                  my $atts = \%{$tags[$i]->{atts}} || {};
                  my %atts= map { _repl_match( $_, @matches) => _repl_match( $atts->{$_}, @matches) } keys %$atts;
                  $elt= $elt->insert_new_elt( after => $tag, \%atts, $match);
                  push @result, $elt;
                  $i= ($i + 1) % @tags;
                }
            }
          else
            { # match, no captures
              my $tag  = $tags[0]->{tag};
              my $atts = \%{$tags[0]->{atts}} || {};
              $elt=  $elt->insert_new_elt( after => $tag, $atts);
              push @result, $elt;
            }
        }
      if( $previous_match && $text)
        { # there was at least 1 match, and there is text left after the match
          $elt= $elt->insert_new_elt( after => $gi, $text);
        }

      push @result, $elt if( $return_all);

      return @result; # return all elements
   }

sub _repl_match
  { my( $val, @matches)= @_;
    $val=~ s{\$(\d+)}{$matches[$1-1]}g;
    return $val;
  }

  # evil hack needed as sometimes 
  my $encode_is_loaded=0;   # so we only load Encode once
  sub _utf8_ify
    { 
      if( $perl_version >= 5.008 and $perl_version < 5.010 and !_keep_encoding()) 
        { unless( $encode_is_loaded) { require Encode; import Encode; $encode_is_loaded++; }
          Encode::_utf8_on( $_[0]); # the flag should be set but is not
        }
    }


}

{ my %replace_sub; # cache for complex expressions (expression => sub)

  sub subs_text
    { my( $elt, $regexp, $replace)= @_;
  
      my $replacement_string;
      my $is_string= _is_string( $replace);
      foreach my $text_elt ($elt->descendants_or_self( $TEXT))
        { 
          if( $is_string)
            { my $text= $text_elt->text;
              $text=~ s{$regexp}{ _replace_var( $replace, $1, $2, $3, $4, $5, $6, $7, $8, $9)}egx;
              $text_elt->set_text( $text);
           }
          else
            {  
              no utf8; # = perl 5.6
              my $replace_sub= ( $replace_sub{$replace} ||= _install_replace_sub( $replace)); 
              my $text= $text_elt->text;
              my $pos=0;  # used to skip text that was previously matched
              my $found_hit;
              while( my( $pre_match_string, $match_string, @var)= ($text=~ m{(.*?)($regexp)}sg))
                { $found_hit=1;
                  my $match_start  = length( $pre_match_string);
                  my $match        = $match_start ? $text_elt->split_at( $match_start + $pos) : $text_elt;
                  my $match_length = length( $match_string);
                  my $post_match   = $match->split_at( $match_length); 
                  $replace_sub->( $match, @var);
                  # merge previous text with current one
                  my $next_sibling;
                  if(    ($next_sibling= $text_elt->{next_sibling})
                      && ($XML::Twig::index2gi[$text_elt->{'gi'}] eq $XML::Twig::index2gi[$next_sibling->{'gi'}])
                    )
                    { $text_elt->merge_text( $next_sibling); }
                    
                  # if the match is at the beginning of the text an empty #PCDATA is left: remove it 
                  if( !$text_elt->text) { $text_elt->delete; } 
                  
                  # go to next 
                  $text_elt= $post_match;
                  $text= $post_match->text;

                  # if the match is at the end of the text an empty #PCDATA is left: remove it 
                  if( !$text_elt->text) { $text_elt->delete; } 
                  
                }
              if( $found_hit) { $text_elt->normalize; } # in case consecutive #PCDATA have been created 
              
            }
        }
      return $elt;
    }


  sub _is_string
    { return ($_[0]=~ m{&e[ln]t}) ? 0: 1 }

  sub _replace_var
    { my( $string, @var)= @_;
      unshift @var, undef;
      $string=~ s{\$(\d)}{$var[$1]}g;
      return $string;
    }

  sub _install_replace_sub
    { my $replace_exp= shift;
      my @item= split m{(&e[ln]t\s*\([^)]*\))}, $replace_exp;
      my $sub= q{ my( $match, @var)= @_; my $new; my $last_inserted=$match;};
      my( $gi, $exp);
      foreach my $item (@item)
        { next if ! length $item;
          if(    $item=~ m{^&elt\s*\(([^)]*)\)})
            { $exp= $1; }
          elsif( $item=~ m{^&ent\s*\(\s*([^\s)]*)\s*\)})
            { $exp= " '#ENT' => $1"; }
          else
            { $exp= qq{ '#PCDATA' => "$item"}; }
          $exp=~ s{\$(\d)}{my $i= $1-1; "\$var[$i]"}eg; # replace references to matches
          $sub.= qq{ \$new= \$match->new( $exp); };
          $sub .= q{ $new->paste( after => $last_inserted); $last_inserted=$new;};
        }
      $sub .= q{ $match->delete; };
      #$sub=~ s/;/;\n/g; warn "subs: $sub"; 
      my $coderef= eval "sub { $NO_WARNINGS; $sub }";
      if( $@) { croak( "invalid replacement expression $replace_exp: ",$@); }
      return $coderef;
    }

  }


sub merge_text
  { my( $e1, $e2)= @_;
    croak "invalid merge: can only merge 2 elements" 
        unless( isa( $e2, 'XML::Twig::Elt'));
    croak "invalid merge: can only merge 2 text elements" 
        unless( $e1->is_text && $e2->is_text && ($e1->gi eq $e2->gi));

    my $text1= $e1->text; if( ! defined $text1) { $text1= ''; }
    my $text2= $e2->text; if( ! defined $text2) { $text2= ''; }

    $e1->set_text( $text1 . $text2);

    my $extra_data= $e1->_extra_data_before_end_tag . $e2->extra_data;
    if( $extra_data) 
      { $e1->_del_extra_data_before_end_tag;
        $e1->_push_extra_data_in_pcdata( $extra_data, length( $text1)); 
      }

    if( $extra_data= $e2->_extra_data_in_pcdata)
      { foreach my $data (@$extra_data) { $e1->_push_extra_data_in_pcdata( $data->{text}, $data->{offset} + length( $text1)); } }

    if( my $extra_data_before_end_tag= $e2->_extra_data_before_end_tag) 
      { $e1->_set_extra_data_before_end_tag( $extra_data_before_end_tag); }

    $e2->delete;

    return $e1;
  }

sub merge
  { my( $e1, $e2)= @_;
    my @e2_children= $e2->_children;
    if(     $e1->_last_child && $e1->_last_child->is_pcdata
        &&  @e2_children && $e2_children[0]->is_pcdata
      )
      { $e1->_last_child->{pcdata} .= $e2_children[0]->{pcdata}; shift @e2_children; }
    foreach my $e (@e2_children) { $e->move( last_child => $e1); } 
    $e2->delete;
    return $e1;
  }


# recursively copy an element and returns the copy (can be huge and long)
sub copy
  { my $elt= shift;
    my $copy= $elt->new( $XML::Twig::index2gi[$elt->{'gi'}]);

    if( $elt->extra_data) { $copy->set_extra_data( $elt->extra_data); }
    if( $elt->{extra_data_before_end_tag}) { $copy->_set_extra_data_before_end_tag( $elt->{extra_data_before_end_tag}); }

    if( $elt->is_asis)   { $copy->set_asis; }

    if( (exists $elt->{'pcdata'})) 
      { $copy->{pcdata}= (delete $copy->{empty} || 1) &&  $elt->{pcdata}; 
        if( $elt->{extra_data_in_pcdata}) { $copy->_set_extra_data_in_pcdata( $elt->{extra_data_in_pcdata}); }
      }
    elsif( (exists $elt->{'cdata'}))
      { $copy->_set_cdata( $elt->{cdata}); 
        if( $elt->{extra_data_in_pcdata}) { $copy->_set_extra_data_in_pcdata( $elt->{extra_data_in_pcdata}); }
      }
    elsif( (exists $elt->{'target'}))
      { $copy->_set_pi( $elt->{target}, $elt->{data}); }
    elsif( (exists $elt->{'comment'}))
      { $copy->_set_comment( $elt->{comment}); }
    elsif( (exists $elt->{'ent'}))
      { $copy->{ent}=  $elt->{ent}; }
    else
      { my @children= do { my $elt= $elt; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; };
        if( my $atts= $elt->{att})
          { my %atts;
            tie %atts, 'Tie::IxHash' if (keep_atts_order());
            %atts= %{$atts}; # we want to do a real copy of the attributes
            $copy->set_atts( \%atts);
          }
        foreach my $child (@children)
          { my $child_copy= $child->copy;
            $child_copy->paste( 'last_child', $copy);
          }
      }
    # save links to the original location, which can be convenient and is used for namespace resolution
    foreach my $link ( qw(parent prev_sibling next_sibling) )
      { $copy->{former}->{$link}= $elt->{$link};
        if( $XML::Twig::weakrefs) { weaken( $copy->{former}->{$link}); }
      }

    $copy->{empty}=  $elt->{'empty'};

    return $copy;
  }


sub delete
  { my $elt= shift;
    $elt->cut;
    $elt->DESTROY unless( $XML::Twig::weakrefs);
    return undef;
  }

{ 
  sub DESTROY
    { my $elt= shift;
      return if( $XML::Twig::weakrefs);
      my $t= shift || $elt->twig; # optional argument, passed in recursive calls

      foreach( @{[$elt->_children]}) { $_->DESTROY( $t); }

      # the id reference needs to be destroyed
      # lots of tests to avoid warnings during the cleanup phase
      $elt->del_id( $t) if( $ID && $t && defined( $elt->{att}) && exists( $elt->{att}->{$ID}));
      undef $elt;
    }
}


# ignores the element
sub ignore
  { my $elt= shift;
    my $t= $elt->twig;
    $t->ignore( $elt, @_);
  }

BEGIN {
  my $pretty                    = 0;
  my $quote                     = '"';
  my $INDENT                    = '  ';
  my $empty_tag_style           = 0;
  my $remove_cdata              = 0;
  my $keep_encoding             = 0;
  my $expand_external_entities  = 0;
  my $keep_atts_order           = 0;
  my $do_not_escape_amp_in_atts = 0;
  my $WRAP                      = '80';
  my $REPLACED_ENTS             = qq{&<};

  my ($NSGMLS, $NICE, $INDENTED, $INDENTEDCT, $INDENTEDC, $WRAPPED, $RECORD1, $RECORD2, $INDENTEDA)= (1..9);
  my %KEEP_TEXT_TAG_ON_ONE_LINE= map { $_ => 1 } ( $INDENTED, $INDENTEDCT, $INDENTEDC, $INDENTEDA, $WRAPPED);
  my %WRAPPED =  map { $_ => 1 } ( $WRAPPED, $INDENTEDA, $INDENTEDC);

  my %pretty_print_style=
    ( none       => 0,          # no added \n
      nsgmls     => $NSGMLS,    # nsgmls-style, \n in tags
      # below this line styles are UNSAFE (the generated XML can be well-formed but invalid)
      nice       => $NICE,      # \n after open/close tags except when the 
                                # element starts with text
      indented   => $INDENTED,  # nice plus idented
      indented_close_tag   => $INDENTEDCT,  # nice plus idented
      indented_c => $INDENTEDC, # slightly more compact than indented (closing
                                # tags are on the same line)
      wrapped    => $WRAPPED,   # text is wrapped at column 
      record_c   => $RECORD1,   # for record-like data (compact)
      record     => $RECORD2,   # for record-like data  (not so compact)
      indented_a => $INDENTEDA, # nice, indented, and with attributes on separate
                                # lines as the nsgmls style, as well as wrapped
                                # lines - to make the xml friendly to line-oriented tools
      cvs        => $INDENTEDA, # alias for indented_a
    );

  my ($HTML, $EXPAND)= (1..2);
  my %empty_tag_style=
    ( normal => 0,        # <tag/>
      html   => $HTML,    # <tag />
      xhtml  => $HTML,    # <tag />
      expand => $EXPAND,  # <tag></tag>
    );

  my %quote_style=
    ( double  => '"',    
      single  => "'", 
      # smart  => "smart", 
    );

  my $xml_space_preserve; # set when an element includes xml:space="preserve"

  my $output_filter;      # filters the entire output (including < and >)
  my $output_text_filter; # filters only the text part (tag names, attributes, pcdata)

  my $replaced_ents= $REPLACED_ENTS;


  # returns those pesky "global" variables so you can switch between twigs 
  sub global_state ## no critic (Subroutines::ProhibitNestedSubs);
    { return
       { pretty                    => $pretty,
         quote                     => $quote,
         indent                    => $INDENT,
         empty_tag_style           => $empty_tag_style,
         remove_cdata              => $remove_cdata,
         keep_encoding             => $keep_encoding,
         expand_external_entities  => $expand_external_entities,
         output_filter             => $output_filter,
         output_text_filter        => $output_text_filter,
         keep_atts_order           => $keep_atts_order,
         do_not_escape_amp_in_atts => $do_not_escape_amp_in_atts,
         wrap                      => $WRAP,
         replaced_ents             => $replaced_ents,
        };
    }

  # appdynamicsores the global variables
  sub set_global_state
    { my $state= shift;
      $pretty                    = $state->{pretty};
      $quote                     = $state->{quote};
      $INDENT                    = $state->{indent};
      $empty_tag_style           = $state->{empty_tag_style};
      $remove_cdata              = $state->{remove_cdata};
      $keep_encoding             = $state->{keep_encoding};
      $expand_external_entities  = $state->{expand_external_entities};
      $output_filter             = $state->{output_filter};
      $output_text_filter        = $state->{output_text_filter};
      $keep_atts_order           = $state->{keep_atts_order};
      $do_not_escape_amp_in_atts = $state->{do_not_escape_amp_in_atts};
      $WRAP                      = $state->{wrap};
      $replaced_ents             = $state->{replaced_ents},
    }

  # sets global state to defaults
  sub init_global_state
    { set_global_state(
       { pretty                    => 0,
         quote                     => '"',
         indent                    => $INDENT,
         empty_tag_style           => 0,
         remove_cdata              => 0,
         keep_encoding             => 0,
         expand_external_entities  => 0,
         output_filter             => undef,
         output_text_filter        => undef,
         keep_atts_order           => undef,
         do_not_escape_amp_in_atts => 0,
         wrap                      => $WRAP,
         replaced_ents             => $REPLACED_ENTS,
        });
    }


  # set the pretty_print style (in $pretty) and returns the old one
  # can be called from outside the package with 2 arguments (elt, style)
  # or from inside with only one argument (style)
  # the style can be either a string (one of the keys of %pretty_print_style
  # or a number (presumably an old value saved)
  sub set_pretty_print
    { my $style= lc( defined $_[1] ? $_[1] : $_[0]); # so we cover both cases 
      my $old_pretty= $pretty;
      if( $style=~ /^\d+$/)
        { croak "invalid pretty print style $style" unless( $style < keys %pretty_print_style);
          $pretty= $style;
        }
      else
        { croak "invalid pretty print style '$style'" unless( exists $pretty_print_style{$style});
          $pretty= $pretty_print_style{$style};
        }
      if( $WRAPPED{$pretty} )
        { XML::Twig::_use( 'Text::Wrap') or croak( "Text::Wrap not available, cannot use style $style"); }
      return $old_pretty;
    }
 
  sub _pretty_print { return $pretty; } 
  
  # set the empty tag style (in $empty_tag_style) and returns the old one
  # can be called from outside the package with 2 arguments (elt, style)
  # or from inside with only one argument (style)
  # the style can be either a string (one of the keys of %empty_tag_style
  # or a number (presumably an old value saved)
  sub set_empty_tag_style
    { my $style= lc( defined $_[1] ? $_[1] : $_[0]); # so we cover both cases 
      my $old_style= $empty_tag_style;
      if( $style=~ /^\d+$/)
        { croak "invalid empty tag style $style"
        unless( $style < keys %empty_tag_style);
        $empty_tag_style= $style;
        }
      else
        { croak "invalid empty tag style '$style'"
            unless( exists $empty_tag_style{$style});
          $empty_tag_style= $empty_tag_style{$style};
        }
      return $old_style;
    }

  sub _pretty_print_styles
    { return (sort { $pretty_print_style{$a} <=> $pretty_print_style{$b} || $a cmp $b } keys %pretty_print_style); }
      
  sub set_quote
    { my $style= $_[1] || $_[0];
      my $old_quote= $quote;
      croak "invalid quote '$style'" unless( exists $quote_style{$style});
      $quote= $quote_style{$style};
      return $old_quote;
    }
    
  sub set_remove_cdata
    { my $new_value= defined $_[1] ? $_[1] : $_[0];
      my $old_value= $remove_cdata;
      $remove_cdata= $new_value;
      return $old_value;
    }
      
      
  sub set_indent
    { my $new_value= defined $_[1] ? $_[1] : $_[0];
      my $old_value= $INDENT;
      $INDENT= $new_value;
      return $old_value;
    }

  sub set_wrap
    { my $new_value= defined $_[1] ? $_[1] : $_[0];
      my $old_value= $WRAP;
      $WRAP= $new_value;
      return $old_value;
    }
       
       
  sub set_keep_encoding
    { my $new_value= defined $_[1] ? $_[1] : $_[0];
      my $old_value= $keep_encoding;
      $keep_encoding= $new_value;
      return $old_value;
   }

  sub set_replaced_ents
    { my $new_value= defined $_[1] ? $_[1] : $_[0];
      my $old_value= $replaced_ents;
      $replaced_ents= $new_value;
      return $old_value;
   }

  sub do_not_escape_gt
    { my $old_value= $replaced_ents;
      $replaced_ents= q{&<}; # & needs to be first
      return $old_value;
    }

  sub escape_gt
    { my $old_value= $replaced_ents;
      $replaced_ents= qq{&<>}; # & needs to be first
      return $old_value;
    }

  sub _keep_encoding { return $keep_encoding; } # so I can use elsewhere in the module

  sub set_do_not_escape_amp_in_atts
    { my $new_value= defined $_[1] ? $_[1] : $_[0];
      my $old_value= $do_not_escape_amp_in_atts;
      $do_not_escape_amp_in_atts= $new_value;
      return $old_value;
   }

  sub output_filter      { return $output_filter; }
  sub output_text_filter { return $output_text_filter; }

  sub set_output_filter
    { my $new_value= defined $_[1] ? $_[1] : $_[0]; # can be called in object/non-object mode
      # if called in object mode with no argument, the filter is undefined
      if( isa( $new_value, 'XML::Twig::Elt') || isa( $new_value, 'XML::Twig')) { undef $new_value; }
      my $old_value= $output_filter;
      if( !$new_value || isa( $new_value, 'CODE') )
        { $output_filter= $new_value; }
      elsif( $new_value eq 'latin1')
        { $output_filter= XML::Twig::latin1();
        }
      elsif( $XML::Twig::filter{$new_value})
        {  $output_filter= $XML::Twig::filter{$new_value}; }
      else
        { croak "invalid output filter '$new_value'"; }
      
      return $old_value;
    }
       
  sub set_output_text_filter
    { my $new_value= defined $_[1] ? $_[1] : $_[0]; # can be called in object/non-object mode
      # if called in object mode with no argument, the filter is undefined
      if( isa( $new_value, 'XML::Twig::Elt') || isa( $new_value, 'XML::Twig')) { undef $new_value; }
      my $old_value= $output_text_filter;
      if( !$new_value || isa( $new_value, 'CODE') )
        { $output_text_filter= $new_value; }
      elsif( $new_value eq 'latin1')
        { $output_text_filter= XML::Twig::latin1();
        }
      elsif( $XML::Twig::filter{$new_value})
        {  $output_text_filter= $XML::Twig::filter{$new_value}; }
      else
        { croak "invalid output text filter '$new_value'"; }
      
      return $old_value;
    }
       
  sub set_expand_external_entities
    { my $new_value= defined $_[1] ? $_[1] : $_[0];
      my $old_value= $expand_external_entities;
      $expand_external_entities= $new_value;
      return $old_value;
    }
       
  sub set_keep_atts_order
    { my $new_value= defined $_[1] ? $_[1] : $_[0];
      my $old_value= $keep_atts_order;
      $keep_atts_order= $new_value;
      return $old_value;
    
   }

  sub keep_atts_order { return $keep_atts_order; } # so I can use elsewhere in the module

  my %html_empty_elt;
  BEGIN { %html_empty_elt= map { $_ => 1} qw( base meta link hr br param img area input col); }

  sub start_tag
    { my( $elt, $option)= @_;
 
 
      return if( $elt->{gi} < $XML::Twig::SPECIAL_GI);

      my $extra_data= $elt->{extra_data} || '';

      my $gi= $XML::Twig::index2gi[$elt->{'gi'}];
      my $att= $elt->{att}; # should be $elt->{att}, optimized into a pure hash look-up

      my $ns_map= $att ? $att->{'#original_gi'} : '';
      if( $ns_map) { $gi= _appdynamicsore_original_prefix( $ns_map, $gi); }
      $gi=~ s{^#default:}{}; # remove default prefix
 
      if( $output_text_filter) { $gi= $output_text_filter->( $gi); }
  
      # get the attribute and their values
      my $att_sep = $pretty==$NSGMLS    ? "\n"
                  : $pretty==$INDENTEDA ? "\n" . $INDENT x ($elt->level+1) . '  '
                  :                       ' '
                  ;

      my $replace_in_att_value= $replaced_ents . $quote;
      if( $option->{escape_gt} && $replaced_ents !~ m{>}) { $replace_in_att_value.= '>'; }

      my $tag;
      my @att_names= grep { !( $_=~ m{^#(?!default:)} ) } $keep_atts_order ?  keys %{$att} : sort keys %{$att};
      if( @att_names)
        { my $atts= join $att_sep, map  { my $output_att_name= $ns_map ? _appdynamicsore_original_prefix( $ns_map, $_) : $_;
                                          if( $output_text_filter)
                                            { $output_att_name=  $output_text_filter->( $output_att_name); }
                                          $output_att_name . '=' . $quote . _att_xml_string( $att->{$_}, $replace_in_att_value) . $quote

                                        } 
                                        @att_names
                                   ;
           if( $pretty==$INDENTEDA && @att_names == 1) { $att_sep= ' '; }
           $tag= "<$gi$att_sep$atts";
        }
      else
        { $tag= "<$gi"; }
  
      $tag .= "\n" if($pretty==$NSGMLS);


      # force empty if suitable HTML tag, otherwise use the value from the input tree
      if( ($empty_tag_style eq $HTML) && !$elt->{first_child} && !$elt->{extra_data_before_end_tag} && $html_empty_elt{$gi})
        { $elt->{empty}= 1; }
      my $empty= defined $elt->{empty} ? $elt->{empty} 
               : $elt->{first_child}    ? 1
               :                         0;

      $tag .= (!$elt->{empty} || $elt->{extra_data_before_end_tag})  ? '>'            # element has content
            : (($empty_tag_style eq $HTML) && $html_empty_elt{$gi}) ? ' />'          # html empty element 
                                                                                     # cvs-friendly format
            : ( $pretty == $INDENTEDA && @att_names > 1)            ? "\n" .  $INDENT x $elt->level . "/>"  
            : ( $pretty == $INDENTEDA && @att_names == 1)           ? " />"  
            : $empty_tag_style                                      ? "></" . $XML::Twig::index2gi[$elt->{'gi'}] . ">" # $empty_tag_style is $HTML or $EXPAND
            :                                                         '/>'
            ;

      if( ( (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 1) eq '#') && (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 9) ne '#default:') )) { $tag= ''; }

#warn "TRACE: ", $tag,": ", Encode::is_utf8( $tag) ? "has flag" : "FLAG NOT SET";

      unless( $pretty) { return defined( $extra_data) ? $extra_data . $tag : $tag;  }

      my $prefix='';
      my $return='';   # '' or \n is to be printed before the tag
      my $indent=0;    # number of indents before the tag

      if( $pretty==$RECORD1)
        { my $level= $elt->level;
          $return= "\n" if( $level < 2);
          $indent= 1 if( $level == 1);
        }

     elsif( $pretty==$RECORD2)
        { $return= "\n";
          $indent= $elt->level;
        }

      elsif( $pretty==$NICE)
        { my $parent= $elt->{parent};
          unless( !$parent || $parent->{contains_text}) 
            { $return= "\n"; }
          $elt->{contains_text}= 1 if( ($parent && $parent->{contains_text})
                                     || $elt->contains_text);
        }

      elsif( $KEEP_TEXT_TAG_ON_ONE_LINE{$pretty})
        { my $parent= $elt->{parent};
          unless( !$parent || $parent->{contains_text}) 
            { $return= "\n"; 
              $indent= $elt->level; 
            }
          $elt->{contains_text}= 1 if( ($parent && $parent->{contains_text})
                                     || $elt->contains_text);
        }

      if( $return || $indent)
        { # check for elements in which spaces should be kept
          my $t= $elt->twig;
          return $extra_data . $tag if( $xml_space_preserve);
          if( $t && $t->{twig_keep_spaces_in})
            { foreach my $ancestor ($elt->ancestors)
                { return $extra_data . $tag if( $t->{twig_keep_spaces_in}->{$XML::Twig::index2gi[$ancestor->{'gi'}]}) }
            }
        
          $prefix= $INDENT x $indent;
          if( $extra_data)
            { $extra_data=~ s{\s+$}{};
              $extra_data=~ s{^\s+}{};
              $extra_data= $prefix .  $extra_data . $return;
            }
        }


      return $return . $extra_data . $prefix . $tag;
    }
  
  sub end_tag
    { my $elt= shift;
      return  '' if(    ($elt->{gi}<$XML::Twig::SPECIAL_GI) 
                     || ($elt->{'empty'} && !$elt->{extra_data_before_end_tag})
                   );
      my $tag= "<";
      my $gi= $XML::Twig::index2gi[$elt->{'gi'}];

      if( my $map= $elt->{'att'}->{'#original_gi'}) { $gi= _appdynamicsore_original_prefix( $map, $gi); }
      $gi=~ s{^#default:}{}; # remove default prefix

      if( $output_text_filter) { $gi= $output_text_filter->( $XML::Twig::index2gi[$elt->{'gi'}]); } 
      $tag .=  "/$gi>";

      $tag = ($elt->{extra_data_before_end_tag} || '') . $tag;

      if( ( (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 1) eq '#') && (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 9) ne '#default:') )) { $tag= ''; }

      return $tag unless $pretty;

      my $prefix='';
      my $return=0;    # 1 if a \n is to be printed before the tag
      my $indent=0;    # number of indents before the tag

      if( $pretty==$RECORD1)
        { $return= 1 if( $elt->level == 0);
        }

     elsif( $pretty==$RECORD2)
        { unless( $elt->contains_text)
            { $return= 1 ;
              $indent= $elt->level;
            }
        }

      elsif( $pretty==$NICE)
        { my $parent= $elt->{parent};
          if( (    ($parent && !$parent->{contains_text}) || !$parent )
            && ( !$elt->{contains_text}  
             && ($elt->{has_flushed_child} || $elt->{first_child})           
           )
         )
            { $return= 1; }
        }

      elsif( $KEEP_TEXT_TAG_ON_ONE_LINE{$pretty})
        { my $parent= $elt->{parent};
          if( (    ($parent && !$parent->{contains_text}) || !$parent )
            && ( !$elt->{contains_text}  
             && ($elt->{has_flushed_child} || $elt->{first_child})           
           )
         )
            { $return= 1; 
              $indent= $elt->level; 
            }
        }

      if( $return || $indent)
        { # check for elements in which spaces should be kept
          my $t= $elt->twig;
          return $tag if( $xml_space_preserve);
          if( $t && $t->{twig_keep_spaces_in})
            { foreach my $ancestor ($elt, $elt->ancestors)
                { return $tag if( $t->{twig_keep_spaces_in}->{$XML::Twig::index2gi[$ancestor->{'gi'}]}) }
            }
      
          if( $return) { $prefix= ($pretty== $INDENTEDCT) ? "\n$INDENT" : "\n"; }
          $prefix.= $INDENT x $indent;
    }

      # add a \n at the end of the document (after the root element)
      $tag .= "\n" unless( $elt->{parent});
  
      return $prefix . $tag;
    }

  sub _appdynamicsore_original_prefix
    { my( $map, $name)= @_;
      my $prefix= _ns_prefix( $name);
      if( my $original_prefix= $map->{$prefix})
        { if( $original_prefix eq '#default')
            { $name=~ s{^$prefix:}{}; }
          else
            { $name=~ s{^$prefix(?=:)}{$original_prefix}; }
        }
      return $name;
    }

  # buffer used to hold the text to print/sprint, to avoid passing it back and forth between methods
  my @sprint;

  # $elt is an element to print
  # $fh is an optional filehandle to print to
  # $pretty is an optional value, if true a \n is printed after the < of the
  # opening tag
  sub print
    { my $elt= shift;

      my $fh= isa( $_[0], 'GLOB') || isa( $_[0], 'IO::Scalar') ? shift : undef;
      my $old_select= defined $fh ? select $fh : undef;
      print $elt->sprint( @_);
      select $old_select if( defined $old_select);
    }
 
sub print_to_file
  { my( $elt, $filename)= (shift, shift);
    my $out_fh;
    my $mode= $keep_encoding ? '>' : '>:utf8';                                       # >= perl 5.8
    open( $out_fh, $mode, $filename) or _croak( "cannot create file $filename: $!"); # >= perl 5.8
    $elt->print( $out_fh, @_);
    close $out_fh;
    return $elt;
  }
  
  # same as print but does not output the start tag if the element
  # is marked as flushed
  sub flush 
    { my $elt= shift; 
      my $up_to= $_[0] && isa( $_[0], 'XML::Twig::Elt') ? shift : $elt;
      $elt->twig->flush_up_to( $up_to, @_); 
    }
  sub purge
    { my $elt= shift; 
      my $up_to= $_[0] && isa( $_[0], 'XML::Twig::Elt') ? shift : $elt;
      $elt->twig->purge_up_to( $up_to, @_); 
    }
  
  sub _flush
    { my $elt= shift;
  
      my $pretty;
      my $fh=  isa( $_[0], 'GLOB') || isa( $_[0], 'IO::Scalar') ? shift : undef;
      my $old_select= defined $fh ? select $fh : undef;
      my $old_pretty= defined ($pretty= shift) ? set_pretty_print( $pretty) : undef;

      $xml_space_preserve= 1 if( ($elt->inherit_att( 'xml:space') || '') eq 'preserve');

      $elt->__flush();

      $xml_space_preserve= 0;

      select $old_select if( defined $old_select);
      set_pretty_print( $old_pretty) if( defined $old_pretty);
    }

  sub __flush
    { my $elt= shift;
  
      if( $elt->{gi} >= $XML::Twig::SPECIAL_GI)
        { my $preserve= ($elt->{'att'}->{'xml:space'} || '') eq 'preserve';
          $xml_space_preserve++ if $preserve;
          unless( $elt->_flushed)
            { print $elt->start_tag();
            }
      
          # flush the children
          my @children= do { my $elt= $elt; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; };
          foreach my $child (@children)
            { $child->_flush( $pretty); }
          unless( $elt->{end_tag_flushed}) { print $elt->end_tag; }
          $xml_space_preserve-- if $preserve;
          # used for pretty printing
          if( my $parent= $elt->{parent}) { $parent->{has_flushed_child}= 1; }
        }
      else # text or special element
        { my $text;
          if( (exists $elt->{'pcdata'}))     { $text= $elt->pcdata_xml_string; 
                                     if( my $parent= $elt->{parent}) 
                                       { $parent->{contains_text}= 1; }
                                   }
          elsif( (exists $elt->{'cdata'}))   { $text= $elt->cdata_string;        
                                     if( my $parent= $elt->{parent}) 
                                       { $parent->{contains_text}= 1; }
                                   }
          elsif( (exists $elt->{'target'}))      { $text= $elt->pi_string;          }
          elsif( (exists $elt->{'comment'})) { $text= $elt->comment_string;     }
          elsif( (exists $elt->{'ent'}))     { $text= $elt->ent_string;         }

          print $output_filter ? $output_filter->( $text) : $text;
        }
    }
  

  sub xml_text
    { my( $elt, @options)= @_;

      if( @options && grep { lc( $_) eq 'no_recurse' } @options) { return $elt->xml_text_only; }

      my $string='';

      if( ($elt->{gi} >= $XML::Twig::SPECIAL_GI) )
        { # sprint the children
          my $child= $elt->{first_child} || '';
          while( $child)
            { $string.= $child->xml_text;
            } continue { $child= $child->{next_sibling}; }
        }
      elsif( (exists $elt->{'pcdata'}))  { $string .= $output_filter ?  $output_filter->($elt->pcdata_xml_string) 
                                                           : $elt->pcdata_xml_string; 
                               }
      elsif( (exists $elt->{'cdata'}))   { $string .= $output_filter ?  $output_filter->($elt->cdata_string)  
                                                           : $elt->cdata_string;      
                               }
      elsif( (exists $elt->{'ent'}))     { $string .= $elt->ent_string; }

      return $string;
    }

  sub xml_text_only
    { return join '', map { $_->xml_text if( $_->is_text || (exists $_->{'ent'})) } $_[0]->_children; }

  # same as print but except... it does not print but rather returns the string
  # if the second parameter is set then only the content is returned, not the
  # start and end tags of the element (but the tags of the included elements are
  # returned)

  sub sprint
    { my $elt= shift;
      my( $old_pretty, $old_empty_tag_style);

      if( $_[0] && isa( $_[0], 'HASH'))
        { my %args= XML::Twig::_normalize_args( %{shift()}); 
          if( defined $args{PrettyPrint}) { $old_pretty          = set_pretty_print( $args{PrettyPrint});  }
           if( defined $args{EmptyTags})  { $old_empty_tag_style = set_empty_tag_style( $args{EmptyTags}); }
        }

      $xml_space_preserve= 1 if( ($elt->inherit_att( 'xml:space') || '') eq 'preserve');

      @sprint=();
      $elt->_sprint( @_);
      my $sprint= join( '', @sprint);
      if( $output_filter) { $sprint= $output_filter->( $sprint); }

      if( ( ($pretty== $WRAPPED) || ($pretty==$INDENTEDC)) && !$xml_space_preserve)
        { $sprint= _wrap_text( $sprint); }
      $xml_space_preserve= 0;


      if( defined $old_pretty)          { set_pretty_print( $old_pretty);             } 
      if( defined $old_empty_tag_style) { set_empty_tag_style( $old_empty_tag_style); }

      return $sprint;
    }
  
  sub _wrap_text
    { my( $string)= @_;
      my $wrapped;
      foreach my $line (split /\n/, $string)
        { my( $initial_indent)= $line=~ m{^(\s*)};
          my $wrapped_line= Text::Wrap::wrap(  '',  $initial_indent . $INDENT, $line) . "\n";
          
          # fix glitch with Text::wrap when the first line is long and does not include spaces
          # the first line ends up being too short by 2 chars, but we'll have to live with it!
          $wrapped_line=~ s{^ +\n  }{}s; # this prefix needs to be removed
      
          $wrapped .= $wrapped_line;
        }
     
      return $wrapped;
    }
      
  
  sub _sprint
    { my $elt= shift;
      my $no_tag= shift || 0;
      # in case there's some comments or PI's piggybacking

      if( $elt->{gi} >= $XML::Twig::SPECIAL_GI)
        {
          my $preserve= ($elt->{'att'}->{'xml:space'} || '') eq 'preserve';
          $xml_space_preserve++ if $preserve;

          push @sprint, $elt->start_tag unless( $no_tag);
      
          # sprint the children
          my $child= $elt->{first_child};
          while( $child)
            { $child->_sprint;
              $child= $child->{next_sibling};
            }
          push @sprint, $elt->end_tag unless( $no_tag);
          $xml_space_preserve-- if $preserve;
        }
      else
        { push @sprint, $elt->{extra_data} if( $elt->{extra_data}) ;
          if(    (exists $elt->{'pcdata'}))  { push @sprint, $elt->pcdata_xml_string; }
          elsif( (exists $elt->{'cdata'}))   { push @sprint, $elt->cdata_string;      }
          elsif( (exists $elt->{'target'}))      { if( ($pretty >= $INDENTED) && !$elt->{parent}->{contains_text}) { push @sprint, "\n" . $INDENT x $elt->level; }
                                     push @sprint, $elt->pi_string;
                                   }
          elsif( (exists $elt->{'comment'})) { if( ($pretty >= $INDENTED) && !$elt->{parent}->{contains_text}) { push @sprint, "\n" . $INDENT x $elt->level; }
                                     push @sprint, $elt->comment_string;    
                                   }
          elsif( (exists $elt->{'ent'}))     { push @sprint, $elt->ent_string;        }
        }

      return;
    }

  # just a shortcut to $elt->sprint( 1)
  sub xml_string
    { my $elt= shift;
      isa( $_[0], 'HASH') ?  $elt->sprint( shift(), 1) : $elt->sprint( 1);
    }

  sub pcdata_xml_string 
    { my $elt= shift;
      if( defined( my $string= $elt->{pcdata}) )
        { 
          if( ! $elt->{extra_data_in_pcdata})
            { 
              $string=~ s/([$replaced_ents])/$XML::Twig::base_ent{$1}/g unless( $keep_encoding || $elt->{asis});  
              $string=~ s{\Q]]>}{]]&gt;}g;
            }
          else
            { _gen_mark( $string); # used by _(un)?protect_extra_data
              foreach my $data (reverse @{$elt->{extra_data_in_pcdata}})
                { my $substr= substr( $string, $data->{offset});
                  if( $keep_encoding || $elt->{asis})
                    { substr( $string, $data->{offset}, 0, $data->{text}); }
                  else
                    { substr( $string, $data->{offset}, 0, _protect_extra_data( $data->{text})); }
                }
              unless( $keep_encoding || $elt->{asis})
                { 
                  $string=~ s{([$replaced_ents])}{$XML::Twig::base_ent{$1}}g ;
                  $string=~ s{\Q]]>}{]]&gt;}g;
                  _unprotect_extra_data( $string);
                }
            }
          return $output_text_filter ? $output_text_filter->( $string) : $string;
        }
      else
        { return ''; }
    }

  { my $mark;
    my( %char2ent, %ent2char);
    BEGIN
      { %char2ent= ( '<' => 'lt', '&' => 'amp', '>' => 'gt');
        %ent2char= map { $char2ent{$_} => $_ } keys %char2ent;
      }

    # generate a unique mark (a string) not found in the string, 
    # used to mark < and & in the extra data
    sub _gen_mark
      { $mark="AAAA";
        $mark++ while( index( $_[0], $mark) > -1);
        return $mark;
      }
      
    sub _protect_extra_data
      { my( $extra_data)= @_;
        $extra_data=~ s{([<&>])}{:$mark:$char2ent{$1}:}g;
        return $extra_data;
      }

    sub _unprotect_extra_data
      { $_[0]=~ s{:$mark:(\w+):}{$ent2char{$1}}g; }

  } 
  
  sub cdata_string
    { my $cdata= $_[0]->{cdata};
      unless( defined $cdata) { return ''; }
      if( $remove_cdata)
        { $cdata=~ s/([$replaced_ents])/$XML::Twig::base_ent{$1}/g; }
      else
        { $cdata= $CDATA_START . $cdata . $CDATA_END; }
      return $cdata;
   }

  sub att_xml_string 
    { my $elt= shift;
      my $att= shift;

      my $replace= $replaced_ents . $quote;
      if($_[0] && $_[0]->{escape_gt} && ($replace!~ m{>}) ) { $replace .='>'; }

      if( defined (my $string= $elt->{att}->{$att}))
        { return _att_xml_string( $string, $replace); }
      else
        { return ''; }
    }
    
  # escaped xml string for an attribute value
  sub _att_xml_string 
    { my( $string, $escape)= @_;
      if( !defined( $string)) { return ''; }
      unless( $keep_encoding)
        { 
          if( $do_not_escape_amp_in_atts)
            { $escape=~ s{^.}{}; # seems like the most backward compatible way to remove & from the list
              $string=~ s{([$escape])}{$XML::Twig::base_ent{$1}}g; 
              $string=~ s{&(?!(\w+|#\d+|[xX][0-9a-fA-F]+);)}{&amp;}g; # dodgy: escape & that do not start an entity
            }
          else
            { $string=~ s{([$escape])}{$XML::Twig::base_ent{$1}}g; 
              $string=~ s{\Q]]>}{]]&gt;}g;
            }
        }

      return $output_text_filter ? $output_text_filter->( $string) : $string;
    }

  sub ent_string 
    { my $ent= shift;
      my $ent_text= $ent->{ent};
      my( $t, $el, $ent_string);
      if(    $expand_external_entities
          && ($t= $ent->twig) 
          && ($el= $t->entity_list)
          && ($ent_string= $el->{entities}->{$ent->ent_name}->{val})
        )
        { return $ent_string; }
       else
         { return $ent_text;  }
    }

  # returns just the text, no tags, for an element
  sub text
    { my( $elt, @options)= @_;

      if( @options && grep { lc( $_) eq 'no_recurse' } @options) { return $elt->text_only; }
 
      my $string;
  
      if( (exists $elt->{'pcdata'}))     { return  $elt->{pcdata};   }
      elsif( (exists $elt->{'cdata'}))   { return  $elt->{cdata};    }
      elsif( (exists $elt->{'target'}))      { return  $elt->pi_string;}
      elsif( (exists $elt->{'comment'})) { return  $elt->{comment};  }
      elsif( (exists $elt->{'ent'}))     { return  $elt->{ent} ;     }
  
      my $child= $elt->{first_child} ||'';
      while( $child)
        {
          my $child_text= $child->text;
          $string.= defined( $child_text) ? $child_text : '';
        } continue { $child= $child->{next_sibling}; }

      unless( defined $string) { $string=''; }
 
      return $output_text_filter ? $output_text_filter->( $string) : $string;
    }

  sub text_only
    { return join '', map { $_->text if( $_->is_text || (exists $_->{'ent'})) } $_[0]->_children; }

  sub trimmed_text
    { my $elt= shift;
      my $text= $elt->text( @_);
      $text=~ s{\s+}{ }sg;
      $text=~ s{^\s*}{};
      $text=~ s{\s*$}{};
      return $text;
    }

  sub trim
    { my( $elt)= @_;
      my $pcdata= $elt->first_descendant( $TEXT);
      (my $pcdata_text= $pcdata->text)=~ s{^\s+}{}s;
      $pcdata->set_text( $pcdata_text);
      $pcdata= $elt->last_descendant( $TEXT);
      ($pcdata_text= $pcdata->text)=~ s{\s+$}{};
      $pcdata->set_text( $pcdata_text);
      foreach my $pcdata ($elt->descendants( $TEXT))
        { ($pcdata_text= $pcdata->text)=~ s{\s+}{ }g;
          $pcdata->set_text( $pcdata_text);
        }
      return $elt;
    }
  

  # remove cdata sections (turns them into regular pcdata) in an element 
  sub remove_cdata 
    { my $elt= shift;
      foreach my $cdata ($elt->descendants_or_self( $CDATA))
        { if( $keep_encoding)
            { my $data= $cdata->{cdata};
              $data=~ s{([&<"'])}{$XML::Twig::base_ent{$1}}g;
              $cdata->{pcdata}= (delete $cdata->{empty} || 1) &&  $data;
            }
          else
            { $cdata->{pcdata}= (delete $cdata->{empty} || 1) &&  $cdata->{cdata}; }
          $cdata->{gi}=$XML::Twig::gi2index{$PCDATA} or $cdata->set_gi( $PCDATA);
          undef $cdata->{cdata};
        }
    }

sub _is_private      { return _is_private_name( $_[0]->gi); }
sub _is_private_name { return $_[0]=~ m{^#(?!default:)};                }


} # end of block containing package globals ($pretty_print, $quotes, keep_encoding...)

# merges consecutive #PCDATAs in am element
sub normalize
  { my( $elt)= @_;
    my @descendants= $elt->descendants( $PCDATA);
    while( my $desc= shift @descendants)
      { while( @descendants && $desc->{next_sibling} && $desc->{next_sibling}== $descendants[0])
          { my $to_merge= shift @descendants;
            $desc->{pcdata}.= $to_merge->{pcdata};
            $to_merge->delete;
          }
      }
    return $elt;
  }

# SAX export methods
sub toSAX1
  { _toSAX(@_, \&_start_tag_data_SAX1, \&_end_tag_data_SAX1); }

sub toSAX2
  { _toSAX(@_, \&_start_tag_data_SAX2, \&_end_tag_data_SAX2); }

sub _toSAX
  { my( $elt, $handler, $start_tag_data, $end_tag_data)= @_;
    if( $elt->{gi} >= $XML::Twig::SPECIAL_GI)
      { my $data= $start_tag_data->( $elt);
        _start_prefix_mapping( $elt, $handler, $data);
        if( $data && (my $start_element = $handler->can( 'start_element')))
          { unless( $elt->_flushed) { $start_element->( $handler, $data); } }
      
        foreach my $child ($elt->_children)
          { $child->_toSAX( $handler, $start_tag_data, $end_tag_data); }

        if( (my $data= $end_tag_data->( $elt)) && (my $end_element = $handler->can( 'end_element')) )
          { $end_element->( $handler, $data); }
        _end_prefix_mapping( $elt, $handler);
      }
    else # text or special element
      { if( (exists $elt->{'pcdata'}) && (my $characters= $handler->can( 'characters')))
          { $characters->( $handler, { Data => $elt->{pcdata} });  }
        elsif( (exists $elt->{'cdata'}))  
          { if( my $start_cdata= $handler->can( 'start_cdata'))
              { $start_cdata->( $handler); }
            if( my $characters= $handler->can( 'characters'))
              { $characters->( $handler, {Data => $elt->{cdata} });  }
            if( my $end_cdata= $handler->can( 'end_cdata'))
              { $end_cdata->( $handler); }
          }
        elsif( ((exists $elt->{'target'}))  && (my $pi= $handler->can( 'processing_instruction')))
          { $pi->( $handler, { Target =>$elt->{target}, Data => $elt->{data} });  }
        elsif( ((exists $elt->{'comment'}))  && (my $comment= $handler->can( 'comment')))
          { $comment->( $handler, { Data => $elt->{comment} });  }
        elsif( ((exists $elt->{'ent'})))
          { 
            if( my $se=   $handler->can( 'skipped_entity'))
              { $se->( $handler, { Name => $elt->ent_name });  }
            elsif( my $characters= $handler->can( 'characters'))
              { if( defined $elt->ent_string)
                  { $characters->( $handler, {Data => $elt->ent_string});  }
                else
                  { $characters->( $handler, {Data => $elt->ent_name});  }
              }
          }
      
      }
  }
  
sub _start_tag_data_SAX1
  { my( $elt)= @_;
    my $name= $XML::Twig::index2gi[$elt->{'gi'}];
    return if( ( (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 1) eq '#') && (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 9) ne '#default:') ));
    my $attributes={};
    my $atts= $elt->{att};
    while( my( $att, $value)= each %$atts)
      { $attributes->{$att}= $value unless( ( $att=~ m{^#(?!default:)} )); }
    my $data= { Name => $name, Attributes => $attributes};
    return $data;
  }

sub _end_tag_data_SAX1
  { my( $elt)= @_;
    return if( ( (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 1) eq '#') && (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 9) ne '#default:') ));
    return  { Name => $XML::Twig::index2gi[$elt->{'gi'}] };
  } 
  
sub _start_tag_data_SAX2
  { my( $elt)= @_;
    my $data={};
    
    my $name= $XML::Twig::index2gi[$elt->{'gi'}];
    return if( ( (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 1) eq '#') && (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 9) ne '#default:') ));
    $data->{Name}         = $name;
    $data->{Prefix}       = $elt->ns_prefix; 
    $data->{LocalName}    = $elt->local_name;
    $data->{NamespaceURI} = $elt->namespace;

    # save a copy of the data so we can re-use it for the end tag
    my %sax2_data= %$data;
    $elt->{twig_elt_SAX2_data}= \%sax2_data;
   
    # add the attributes
    $data->{Attributes}= $elt->_atts_to_SAX2;

    return $data;
  }

sub _atts_to_SAX2
  { my $elt= shift;
    my $SAX2_atts= {};
    foreach my $att (keys %{$elt->{att}})
      { 
        next if( ( $att=~ m{^#(?!default:)} ));
        my $SAX2_att={};
        $SAX2_att->{Name}         = $att;
        $SAX2_att->{Prefix}       = _ns_prefix( $att); 
        $SAX2_att->{LocalName}    = _local_name( $att);
        $SAX2_att->{NamespaceURI} = $elt->namespace( $SAX2_att->{Prefix});
        $SAX2_att->{Value}        = $elt->{'att'}->{$att};
        my $SAX2_att_name= "{$SAX2_att->{NamespaceURI}}$SAX2_att->{LocalName}";

        $SAX2_atts->{$SAX2_att_name}= $SAX2_att;
      }
    return $SAX2_atts;
  }

sub _start_prefix_mapping
  { my( $elt, $handler, $data)= @_;
    if( my $start_prefix_mapping= $handler->can( 'start_prefix_mapping')
        and my @new_prefix_mappings= grep { /^\{[^}]*\}xmlns/ || /^\{$XMLNS_URI\}/ } keys %{$data->{Attributes}}
      )
      { foreach my $prefix (@new_prefix_mappings)
          { my $prefix_string= $data->{Attributes}->{$prefix}->{LocalName};
            if( $prefix_string eq 'xmlns') { $prefix_string=''; }
            my $prefix_data=
              {  Prefix       => $prefix_string,
                 NamespaceURI => $data->{Attributes}->{$prefix}->{Value}
              };
            $start_prefix_mapping->( $handler, $prefix_data);
            $elt->{twig_end_prefix_mapping} ||= [];
            push @{$elt->{twig_end_prefix_mapping}}, $prefix_string;
          }
      }
  }

sub _end_prefix_mapping
  { my( $elt, $handler)= @_;
    if( my $end_prefix_mapping= $handler->can( 'end_prefix_mapping'))
      { foreach my $prefix (@{$elt->{twig_end_prefix_mapping}})
          { $end_prefix_mapping->( $handler, { Prefix => $prefix} ); }
      }
  }
             
sub _end_tag_data_SAX2
  { my( $elt)= @_;
    return if( ( (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 1) eq '#') && (substr( $XML::Twig::index2gi[$elt->{'gi'}], 0, 9) ne '#default:') ));
    return $elt->{twig_elt_SAX2_data};
  } 

sub contains_text
  { my $elt= shift;
    my $child= $elt->{first_child};
    while ($child)
      { return 1 if( $child->is_text || (exists $child->{'ent'})); 
        $child= $child->{next_sibling};
      }
    return 0;
  }

# creates a single pcdata element containing the text as child of the element
# options: 
#   - force_pcdata: when set to a true value forces the text to be in a #PCDATA
#                   even if the original element was a #CDATA
sub set_text
  { my( $elt, $string, %option)= @_;

    if( $XML::Twig::index2gi[$elt->{'gi'}] eq $PCDATA) 
      { return $elt->{pcdata}= (delete $elt->{empty} || 1) &&  $string; }
    elsif( $XML::Twig::index2gi[$elt->{'gi'}] eq $CDATA)  
      { if( $option{force_pcdata})
          { $elt->{gi}=$XML::Twig::gi2index{$PCDATA} or $elt->set_gi( $PCDATA);
            $elt->_set_cdata('');
            return $elt->{pcdata}= (delete $elt->{empty} || 1) &&  $string;
          }
        else
          { return $elt->_set_cdata( $string); }
      }
    elsif( $elt->contains_a_single( $PCDATA) )
      { # optimized so we have a slight chance of not loosing embedded comments and pi's
        $elt->{first_child}->set_pcdata( $string);
        return $elt;
      }

    foreach my $child (@{[$elt->_children]})
      { $child->delete; }

    my $pcdata= $elt->_new_pcdata( $string);
    $pcdata->paste( $elt);

    $elt->{empty}=0;

    return $elt;
  }

# set the content of an element from a list of strings and elements
sub set_content
  { my $elt= shift;

    return $elt unless defined $_[0];

    # attributes can be given as a hash (passed by ref)
    if( ref $_[0] eq 'HASH')
      { my $atts= shift;
        $elt->del_atts; # usually useless but better safe than sorry
        $elt->set_atts( $atts);
        return $elt unless defined $_[0];
      }

    # check next argument for #EMPTY
    if( !(ref $_[0]) && ($_[0] eq $EMPTY) ) 
      { $elt->{empty}= 1; return $elt; }

    # case where we really want to do a set_text, the element is '#PCDATA'
    # or contains a single PCDATA and we only want to add text in it
    if( ($XML::Twig::index2gi[$elt->{'gi'}] eq $PCDATA || $elt->contains_a_single( $PCDATA)) 
        && (@_ == 1) && !( ref $_[0]))
      { $elt->set_text( $_[0]);
        return $elt;
      }
    elsif( ($XML::Twig::index2gi[$elt->{'gi'}] eq $CDATA) && (@_ == 1) && !( ref $_[0]))
      { $elt->_set_cdata( $_[0]);
        return $elt;
      }

    # delete the children
    foreach my $child (@{[$elt->_children]})
      { $child->delete; }

    if( @_) { $elt->{empty}=0; }

    foreach my $child (@_)
      { if( ref( $child) && isa( $child, 'XML::Twig::Elt'))
          { # argument is an element
            $child->paste( 'last_child', $elt);
          }
        else
          { # argument is a string
            if( (my $pcdata= $elt->{last_child}) && $elt->{last_child}->is_pcdata)
              { # previous child is also pcdata: just concatenate
                $pcdata->{pcdata}= (delete $pcdata->{empty} || 1) &&  $pcdata->{pcdata} . $child 
              }
            else
              { # previous child is not a string: creat a new pcdata element
                $pcdata= $elt->_new_pcdata( $child);
                $pcdata->paste( 'last_child', $elt);  
              }
          }
      }


    return $elt;
  }

# inserts an element (whose gi is given) as child of the element
# all children of the element are now children of the new element
# returns the new element
sub insert
  { my ($elt, @args)= @_;
    # first cut the children
    my @children= do { my $elt= $elt; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; };
    foreach my $child (@children)
      { $child->cut; }
    # insert elements
    while( my $gi= shift @args)
      { my $new_elt= $elt->new( $gi);
        # add attributes if needed
        if( defined( $args[0]) && ( isa( $args[0], 'HASH')) )
          { $new_elt->set_atts( shift @args); }
        # paste the element
        $new_elt->paste( $elt);
        $elt->{empty}=0;
        $elt= $new_elt;
      }
    # paste back the children
    foreach my $child (@children)
      { $child->paste( 'last_child', $elt); }
    return $elt;
  }

# insert a new element 
# $elt->insert_new_element( $opt_position, $gi, $opt_atts_hash, @opt_content); 
# the element is created with the same syntax as new
# position is the same as in paste, first_child by default
sub insert_new_elt
  { my $elt= shift;
    my $position= $_[0];
    if(     ($position eq 'before') || ($position eq 'after')
         || ($position eq 'first_child') || ($position eq 'last_child'))
      { shift; }
    else
      { $position= 'first_child'; }

    my $new_elt= $elt->new( @_);
    $new_elt->paste( $position, $elt);

    #if( defined $new_elt->{'att'}->{$ID}) { $new_elt->set_id( $new_elt->{'att'}->{$ID}); }
    
    return $new_elt;
  }

# wraps an element in elements which gi's are given as arguments
# $elt->wrap_in( 'td', 'tr', 'table') wraps the element as a single
# cell in a table for example
# returns the new element
sub wrap_in
  { my $elt= shift;
    while( my $gi = shift @_)
      { my $new_elt = $elt->new( $gi);
        if( $elt->{twig_current})
          { my $t= $elt->twig;
            $t->{twig_current}= $new_elt;
            delete $elt->{'twig_current'};
            $new_elt->{'twig_current'}=1;
          }

        if( my $parent= $elt->{parent})
          { $new_elt->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $new_elt->{parent});} ; 
            if( $parent->{first_child} == $elt) { $parent->{first_child}=  $new_elt; }
             if( $parent->{last_child} == $elt) {  $parent->{empty}=0; $parent->{last_child}=$new_elt; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});} ;  }
          }
        else
          { # wrapping the root
            my $twig= $elt->twig;
            if( $twig && $twig->root && ($twig->root eq $elt) )
              { $twig->set_root( $new_elt); 
              }
          }

        if( my $prev_sibling= $elt->{prev_sibling})
          { $new_elt->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $new_elt->{prev_sibling});} ;
            $prev_sibling->{next_sibling}=  $new_elt;
          }

        if( my $next_sibling= $elt->{next_sibling})
          { $new_elt->{next_sibling}=  $next_sibling;
            $next_sibling->{prev_sibling}=$new_elt; if( $XML::Twig::weakrefs) { weaken( $next_sibling->{prev_sibling});} ;
          }
        $new_elt->{first_child}=  $elt;
         $new_elt->{empty}=0; $new_elt->{last_child}=$elt; if( $XML::Twig::weakrefs) { weaken( $new_elt->{last_child});} ;

        $elt->{parent}=$new_elt; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
        $elt->{prev_sibling}=undef; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;
        $elt->{next_sibling}=  undef;

        # add the attributes if the next argument is a hash ref
        if( defined( $_[0]) && (isa( $_[0], 'HASH')) )
          { $new_elt->set_atts( shift @_); }

        $elt= $new_elt;
      }
      
    return $elt;
  }

sub replace
  { my( $elt, $ref)= @_;

    if( $elt->{parent}) { $elt->cut; }

    if( my $parent= $ref->{parent})
      { $elt->{parent}=$parent; if( $XML::Twig::weakrefs) { weaken( $elt->{parent});} ;
        if( $parent->{first_child} == $ref) { $parent->{first_child}=  $elt; }
        if( $parent->{last_child} == $ref)  {  $parent->{empty}=0; $parent->{last_child}=$elt; if( $XML::Twig::weakrefs) { weaken( $parent->{last_child});}  ; }
      }
    if( my $prev_sibling= $ref->{prev_sibling})
      { $elt->{prev_sibling}=$prev_sibling; if( $XML::Twig::weakrefs) { weaken( $elt->{prev_sibling});} ;
        $prev_sibling->{next_sibling}=  $elt;
      }
    if( my $next_sibling= $ref->{next_sibling})
      { $elt->{next_sibling}=  $next_sibling;
        $next_sibling->{prev_sibling}=$elt; if( $XML::Twig::weakrefs) { weaken( $next_sibling->{prev_sibling});} ;
      }
   
    $ref->{parent}=undef; if( $XML::Twig::weakrefs) { weaken( $ref->{parent});} ;
    $ref->{prev_sibling}=undef; if( $XML::Twig::weakrefs) { weaken( $ref->{prev_sibling});} ;
    $ref->{next_sibling}=  undef;
    return $ref;
  }

sub replace_with
  { my $ref= shift;
    my $elt= shift;
    $elt->replace( $ref);
    foreach my $new_elt (reverse @_)
      { $new_elt->paste( after => $elt); }
    return $elt;
  }


# move an element, same syntax as paste, except the element is first cut
sub move
  { my $elt= shift;
    $elt->cut;
    $elt->paste( @_);
    return $elt;
  }


# adds a prefix to an element, creating a pcdata child if needed
sub prefix
  { my ($elt, $prefix, $option)= @_;
    my $asis= ($option && ($option eq 'asis')) ? 1 : 0;
    if( (exists $elt->{'pcdata'}) 
        && (($asis && $elt->{asis}) || (!$asis && ! $elt->{asis}))
      )
      { $elt->{pcdata}= (delete $elt->{empty} || 1) &&  $prefix . $elt->{pcdata}; }
    elsif( $elt->{first_child} && $elt->{first_child}->is_pcdata
        && (   ($asis && $elt->{first_child}->{asis}) 
            || (!$asis && ! $elt->{first_child}->{asis}))
         )
      { 
        $elt->{first_child}->set_pcdata( $prefix . $elt->{first_child}->pcdata); 
      }
    else
      { my $new_elt= $elt->_new_pcdata( $prefix);
        $new_elt->paste( $elt);
        if( $asis) { $new_elt->set_asis; }
      }
    return $elt;
  }

# adds a suffix to an element, creating a pcdata child if needed
sub suffix
  { my ($elt, $suffix, $option)= @_;
    my $asis= ($option && ($option eq 'asis')) ? 1 : 0;
    if( (exists $elt->{'pcdata'})
        && (($asis && $elt->{asis}) || (!$asis && ! $elt->{asis}))
      )
      { $elt->{pcdata}= (delete $elt->{empty} || 1) &&  $elt->{pcdata} . $suffix; }
    elsif( $elt->{last_child} && $elt->{last_child}->is_pcdata
        && (   ($asis && $elt->{last_child}->{asis}) 
            || (!$asis && ! $elt->{last_child}->{asis}))
         )
      { $elt->{last_child}->set_pcdata( $elt->{last_child}->pcdata . $suffix); }
    else
      { my $new_elt= $elt->_new_pcdata( $suffix);
        $new_elt->paste( 'last_child', $elt);
        if( $asis) { $new_elt->set_asis; }
      }
    return $elt;
  }

# create a path to an element ('/root/.../gi)
sub path
  { my $elt= shift;
    my @context= ( $elt, $elt->ancestors);
    return "/" . join( "/", reverse map {$_->gi} @context);
  }

sub xpath
  { my $elt= shift;
    my $xpath;
    foreach my $ancestor (reverse $elt->ancestors_or_self)
      { my $gi= $XML::Twig::index2gi[$ancestor->{'gi'}];
        $xpath.= "/$gi";
        my $index= $ancestor->prev_siblings( $gi) + 1;
        unless( ($index == 1) && !$ancestor->next_sibling( $gi))
          { $xpath.= "[$index]"; }
      }
    return $xpath;
  }

# methods used mainly by wrap_children

# return a string with the 
# for an element <foo><elt att="val">...</elt><elt2/><elt>...</elt></foo>
# returns '<elt att="val"><elt2><elt>'
sub _stringify_struct
  { my( $elt, %opt)= @_;
    my $string='';
    my $pretty_print= set_pretty_print( 'none');
    foreach my $child ($elt->_children)
      { $child->add_id; $string .= $child->start_tag( { escape_gt => 1 }) ||''; }
    set_pretty_print( $pretty_print);
    return $string;
  }

# wrap a series of elements in a new one
sub _wrap_range
  { my $elt= shift;
    my $gi= shift;
    my $atts= isa( $_[0], 'HASH') ? shift : undef;
    my $range= shift; # the string with the tags to wrap

    my $t= $elt->twig;

    # get the tags to wrap
    my @to_wrap;
    while( $range=~ m{<\w+\s+[^>]*id=("[^"]*"|'[^']*')[^>]*>}g)
      { push @to_wrap, $t->elt_id( substr( $1, 1, -1)); }

    return '' unless @to_wrap;
    
    my $to_wrap= shift @to_wrap;
    my %atts= %$atts;
    my $new_elt= $to_wrap->wrap_in( $gi, \%atts);
    $_->move( last_child => $new_elt) foreach (@to_wrap);

    return '';
  }
    
# wrap children matching a regexp in a new element
sub wrap_children
  { my( $elt, $regexp, $gi, $atts)= @_;

    $atts ||={};

    my $elt_as_string= $elt->_stringify_struct; # stringify the elt structure
    $regexp=~ s{(<[^>]*>)}{_match_expr( $1)}eg; # in the regexp, replace gi's by the proper regexp 
    $elt_as_string=~ s{($regexp)}{$elt->_wrap_range( $gi, $atts, $1)}eg; # then do the actual replace
  
    return $elt; 
  }

sub _match_expr
  { my $tag= shift;
    my( $gi, %atts)= XML::Twig::_parse_start_tag( $tag);
    return _match_tag( $gi, %atts);
  }


sub _match_tag
  { my( $elt, %atts)= @_;
    my $string= "<$elt\\b";
    foreach my $key (sort keys %atts)
      { my $val= qq{\Q$atts{$key}\E};
        $string.= qq{[^>]*$key=(?:"$val"|'$val')};
      }
    $string.=  qq{[^>]*>};
    return "(?:$string)";
  }

sub field_to_att
  { my( $elt, $cond, $att)= @_;
    $att ||= $cond;
    my $child= $elt->first_child( $cond) or return undef;
    $elt->set_att( $att => $child->text);
    $child->cut;
    return $elt;
  }

sub att_to_field
  { my( $elt, $att, $tag)= @_;
    $tag ||= $att;
    my $child= $elt->insert_new_elt( first_child => $tag, $elt->{'att'}->{$att});
    $elt->del_att( $att);
    return $elt;
  }

# sort children methods

sub sort_children_on_field
  { my $elt   = shift;
    my $field = shift;
    my $get_key= sub { return $_[0]->field( $field) };
    return $elt->sort_children( $get_key, @_); 
  }

sub sort_children_on_att
  { my $elt = shift;
    my $att = shift;
    my $get_key= sub { return $_[0]->{'att'}->{$att} };
    return $elt->sort_children( $get_key, @_); 
  }

sub sort_children_on_value
  { my $elt   = shift;
    #my $get_key= eval qq{ sub { $NO_WARNINGS; return \$_[0]->text } };
    my $get_key= \&text;
    return $elt->sort_children( $get_key, @_); 
  }

sub sort_children
  { my( $elt, $get_key, %opt)=@_;
    $opt{order} ||= 'normal';
    $opt{type}  ||= 'alpha';
    my( $par_a, $par_b)= ($opt{order} eq 'reverse') ? qw( b a) : qw ( a b) ;
    my $op= ($opt{type} eq 'numeric') ? '<=>' : 'cmp' ;
    my @children= $elt->cut_children;
    if( $opt{type} eq 'numeric')
      {  @children= map  { $_->[1] }
                    sort { $a->[0] <=> $b->[0] }
                    map  { [ $get_key->( $_), $_] } @children;
      }
    elsif( $opt{type} eq 'alpha')
      {  @children= map  { $_->[1] }
                    sort { $a->[0] cmp $b->[0] }
                    map  { [ $get_key->( $_), $_] } @children;
      }
    else
      { croak "wrong sort type '$opt{type}', should be either 'alpha' or 'numeric'"; }

    @children= reverse @children if( $opt{order} eq 'reverse');
    $elt->set_content( @children);
  }


# comparison methods

sub before
  { my( $a, $b)=@_;
    if( $a->cmp( $b) == -1) { return 1; } else { return 0; }
  }

sub after
  { my( $a, $b)=@_;
    if( $a->cmp( $b) == 1) { return 1; } else { return 0; }
  }

sub lt
  { my( $a, $b)=@_;
    return 1 if( $a->cmp( $b) == -1);
    return 0;
  }

sub le
  { my( $a, $b)=@_;
    return 1 unless( $a->cmp( $b) == 1);
    return 0;
  }

sub gt
  { my( $a, $b)=@_;
    return 1 if( $a->cmp( $b) == 1);
    return 0;
  }

sub ge
  { my( $a, $b)=@_;
    return 1 unless( $a->cmp( $b) == -1);
    return 0;
  }


sub cmp
  { my( $a, $b)=@_;

    # easy cases
    return  0 if( $a == $b);    
    return  1 if( $a->in($b)); # a in b => a starts after b 
    return -1 if( $b->in($a)); # b in a => a starts before b

    # ancestors does not include the element itself
    my @a_pile= ($a, $a->ancestors); 
    my @b_pile= ($b, $b->ancestors);

    # the 2 elements are not in the same twig
    return undef unless( $a_pile[-1] == $b_pile[-1]);

    # find the first non common ancestors (they are siblings)
    my $a_anc= pop @a_pile;
    my $b_anc= pop @b_pile;

    while( $a_anc == $b_anc) 
      { $a_anc= pop @a_pile;
        $b_anc= pop @b_pile;
      }

    # from there move left and right and figure out the order
    my( $a_prev, $a_next, $b_prev, $b_next)= ($a_anc, $a_anc, $b_anc, $b_anc);
    while()
      { $a_prev= $a_prev->{prev_sibling} || return( -1);
        return 1 if( $a_prev == $b_next);
        $a_next= $a_next->{next_sibling} || return( 1);
        return -1 if( $a_next == $b_prev);
        $b_prev= $b_prev->{prev_sibling} || return( 1);
        return -1 if( $b_prev == $a_next);
        $b_next= $b_next->{next_sibling} || return( -1);
        return 1 if( $b_next == $a_prev);
      }
  }
    
sub _dump
  { my( $elt, $option)= @_; 
  
    my $atts       = defined $option->{atts}       ? $option->{atts}       :  1;
    my $extra      = defined $option->{extra}      ? $option->{extra}      :  0;
    my $short_text = defined $option->{short_text} ? $option->{short_text} : 40;

    my $sp= '| ';
    my $indent= $sp x $elt->level;
    my $indent_sp= '  ' x $elt->level;
    
    my $dump='';
    if( $elt->is_elt)
      { 
        $dump .= $indent  . '|-' . $XML::Twig::index2gi[$elt->{'gi'}];
        
        if( $atts && (my @atts= $elt->att_names) )
          { $dump .= ' ' . join( ' ', map { qq{$_="} . $elt->{'att'}->{$_} . qq{"} } @atts); }

        $dump .= "\n";
        if( $extra) { $dump .= $elt->_dump_extra_data( $indent, $indent_sp, $short_text); }
        $dump .= join( "", map { $_->_dump( $option) } do { my $elt= $elt; my @children=(); my $child= $elt->{first_child}; while( $child) { push @children, $child; $child= $child->{next_sibling}; } @children; });
      }
    else
      { 
        if( (exists $elt->{'pcdata'}))
          { $dump .= "$indent|-PCDATA:  '"  . _short_text( $elt->{pcdata}, $short_text) . "'\n" }
        elsif( (exists $elt->{'ent'}))
          { warn "here"; $dump .= "$indent|-ENTITY:  '" . _short_text( $elt->{ent}, $short_text) . "'\n" }
        elsif( (exists $elt->{'cdata'}))
          { $dump .= "$indent|-CDATA:   '" . _short_text( $elt->{cdata}, $short_text) . "'\n" }
        elsif( (exists $elt->{'comment'}))
          { $dump .= "$indent|-COMMENT: '" . _short_text( $elt->comment_string, $short_text) . "'\n" }
        elsif( (exists $elt->{'target'}))
          { $dump .= "$indent|-PI:      '"      . $elt->{target} . "' - '" . _short_text( $elt->{data}, $short_text) . "'\n" }
        if( $extra) { $dump .= $elt->_dump_extra_data( $indent, $indent_sp, $short_text); }
      }
    return $dump;
  }

sub _dump_extra_data
  { my( $elt, $indent, $indent_sp, $short_text)= @_;
    my $dump='';
    if( $elt->extra_data)
      { my $extra_data = $indent . "|-- (cpi before) '" . _short_text( $elt->extra_data, $short_text) . "'";
        $extra_data=~ s{\n}{$indent_sp}g;
        $dump .= $extra_data . "\n";
      }
    if( $elt->{extra_data_in_pcdata})
      { foreach my $data ( @{$elt->{extra_data_in_pcdata}})
          { my $extra_data = $indent . "|-- (cpi offset $data->{offset}) '" . _short_text( $data->{text}, $short_text) . "'";
            $extra_data=~ s{\n}{$indent_sp}g;
            $dump .= $extra_data . "\n";
          }
      } 
    if( $elt->{extra_data_before_end_tag})
      { my $extra_data = $indent . "|-- (cpi end) '" . _short_text( $elt->{extra_data_before_end_tag}, $short_text) . "'";
        $extra_data=~ s{\n}{$indent_sp}g;
        $dump .= $extra_data . "\n";
      } 
    return $dump;
  }
 

sub _short_text
  { my( $string, $length)= @_;
    if( !$length || (length( $string) < $length) ) { return $string; }
    my $l1= (length( $string) -5) /2;
    my $l2= length( $string) - ($l1 + 5);
    return substr( $string, 0, $l1) . ' ... ' . substr( $string, -$l2);
  }


sub _and { return _join_defined( ' && ',  @_); }
sub _join_defined { return join( shift(), grep { $_ } @_); }

1;
