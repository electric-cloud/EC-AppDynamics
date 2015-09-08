my %GetAppStatus = (
        label       => "AppDynamics - GetAppStatus", 
        procedure   => "GetAppStatus", 
        description => "Retrieves status of application from AppDynamics Controller", 
        category    => "APM"
   );
my %ManageResources = (
        label       => "AppDynamics - ManageResources", 
        procedure   => "ManageResources", 
        description => "Dynamically manages resources of a business application based on data from AppDynamics Controller", 
        category    => "APM"
    );

$batch->deleteProperty("/server/ec_customEditors/pickerStep/AppDynamics");
$batch->deleteProperty("/server/ec_customEditors/pickerStep/AppDynamics - GetAppStatus");
$batch->deleteProperty("/server/ec_customEditors/pickerStep/AppDynamics - ManageResources");

@::createStepPickerSteps = (\%GetAppStatus, \%ManageResources);