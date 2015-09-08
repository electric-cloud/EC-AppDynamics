@files = (
    ['//procedure[procedureName="CreateConfiguration"]/propertySheet/property[propertyName="ec_parameterForm"]/value', 'forms/createConfigForm.xml'],
    ['//property[propertyName="forms"]/propertySheet/property[propertyName="CreateConfigForm"]/value',              'forms/createConfigForm.xml'],
    ['//property[propertyName="forms"]/propertySheet/property[propertyName="EditConfigForm"]/value',                'forms/editConfigForm.xml'],
    ['//property[propertyName="restDriver.pm"]/value', 'lib/restDriver.pm'],
    ['//property[propertyName="XML::Twig"]/value', 'lib/Twig.pm'],
    ['//property[propertyName="ec_setup"]/value', 'ec_setup.pl'],
    ['//property[propertyName="postp_matchers"]/value', 'postp_matchers.pl'],

    # Aux lib
    ['//property[propertyName="EC::Plugin::Core"]/value', 'lib/EC/Plugin/Core.pm'],
    
    ['//procedure[procedureName="GetAppStatus"]/step[stepName="GetAppStatus"]/command' , 'GetAppStatus.pl'],
    ['//procedure[procedureName="ManageResources"]/step[stepName="ManageResources"]/command' , 'ManageResources.pl'],

    ['//procedure[procedureName="CreateConfiguration"]/step[stepName="CreateConfiguration"]/command',                 'config/createcfg.pl'],
    ['//procedure[procedureName="CreateConfiguration"]/step[stepName="CreateAndAttachCredential"]/command',           'config/createAndAttachCredential.pl'],
    ['//procedure[procedureName="DeleteConfiguration"]/step[stepName="DeleteConfiguration"]/command',                 'config/deletecfg.pl'],
);
