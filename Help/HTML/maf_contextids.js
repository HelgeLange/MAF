var hmContextIds = new Array();
function hmGetContextId(query) {
    var urlParams;
    var match,
        pl = /\+/g,
        search = /([^&=]+)=?([^&]*)/g,
        decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
    params = {};
    while (match = search.exec(query))
       params[decode(match[1])] = decode(match[2]);
    if (params["contextid"]) return decodeURIComponent(hmContextIds[params["contextid"]]);
    else return "";
}

hmContextIds["107"]="introduction.htm";
hmContextIds["106"]="getting_support.htm";
hmContextIds["105"]="deployment.htm";
hmContextIds["108"]="reference.htm";
hmContextIds["130"]="tmafmanagerloader.htm";
hmContextIds["154"]="tmafmanagerloader_properties.htm";
hmContextIds["133"]="tmafmanagerloader_autoload.htm";
hmContextIds["140"]="tmafmanagerloader_dataloadmethod.htm";
hmContextIds["144"]="tmafmanagerloader_inisettings.htm";
hmContextIds["145"]="tmafmanagerloader_manager.htm";
hmContextIds["146"]="tmafmanagerloader_managerloaderoptions.htm";
hmContextIds["147"]="tmafmanagerloader_managersubdirectory.htm";
hmContextIds["155"]="tmafmanagerloader_registrysettings.htm";
hmContextIds["148"]="tmafmanagerloader_methods.htm";
hmContextIds["131"]="tmafmanagerloader_addmanager.htm";
hmContextIds["141"]="tmafmanagerloader_deletemanager.htm";
hmContextIds["139"]="tmafmanagerloader_connectmanager.htm";
hmContextIds["142"]="tmafmanagerloader_disconnectmanager.htm";
hmContextIds["143"]="tmafmanagerloader_events.htm";
hmContextIds["132"]="tmafmanagerloader_afterconnect.htm";
hmContextIds["134"]="tmafmanagerloader_beforeconnect.htm";
hmContextIds["135"]="tmafmanagerloader_beforeloadingmanager.htm";
hmContextIds["149"]="tmafmanagerloader_onconnect.htm";
hmContextIds["150"]="tmafmanagerloader_ondataload.htm";
hmContextIds["151"]="tmafmanagerloader_ondisconnect.htm";
hmContextIds["152"]="tmafmanagerloader_onloadingfinished.htm";
hmContextIds["153"]="tmafmanagerloader_onloadmanagererror.htm";
hmContextIds["157"]="tmafmanagerloader_types.htm";
hmContextIds["156"]="tmafmanagerloader_tmanagernotify.htm";
hmContextIds["136"]="tmafmanagerloader_classes.htm";
hmContextIds["137"]="tmafmanagerloader_classes_inisettings.htm";
hmContextIds["138"]="tmafmanagerloader_classes_registrysettings.htm";
hmContextIds["123"]="tmafhookmanager.htm";
hmContextIds["124"]="tmafhookmanager_properties.htm";
hmContextIds["125"]="tmafhookmanager_types.htm";
hmContextIds["158"]="tmafresourcemanager.htm";
hmContextIds["161"]="tmafresourcemanager_properties.htm";
hmContextIds["160"]="tmafresourcemanager_methods.htm";
hmContextIds["159"]="tmafresourcemanager_events.htm";
hmContextIds["162"]="tmafresourcemanager_types.htm";
hmContextIds["163"]="tmafwindowmanager.htm";
hmContextIds["166"]="tmafwindowmanager_properties.htm";
hmContextIds["165"]="tmafwindowmanager_methods.htm";
hmContextIds["164"]="tmafwindowmanager_events.htm";
hmContextIds["167"]="tmafwindowmanager_types.htm";
hmContextIds["109"]="tmafdatastorage.htm";
hmContextIds["112"]="tmafdatastorage_properties.htm";
hmContextIds["111"]="tmafdatastorage_methods.htm";
hmContextIds["110"]="tmafdatastorage_events.htm";
hmContextIds["113"]="tmafdatastorage_types.htm";
hmContextIds["127"]="tmaflinkmanager.htm";
hmContextIds["128"]="tmaflinkmanager_properties.htm";
hmContextIds["129"]="tmaflinkmanager_types.htm";
hmContextIds["114"]="tmafhooclient.htm";
hmContextIds["122"]="tmafhookclient_properties.htm";
hmContextIds["120"]="tmafhookclient_methods.htm";
hmContextIds["115"]="tmafhooclient_executehook.htm";
hmContextIds["117"]="tmafhookclient___query_hookmanager.htm";
hmContextIds["118"]="tmafhookclient___query_manager.htm";
hmContextIds["126"]="tmafhoookclient___add_functionobserver.htm";
hmContextIds["116"]="tmafhookclient___delete_functionobserver.htm";
hmContextIds["119"]="tmafhookclient_events.htm";
hmContextIds["121"]="tmafhookclient_onfunctionobserver.htm";
hmContextIds["100"]="definitions.htm";
hmContextIds["101"]="definitions_dft.htm";
hmContextIds["102"]="definitions_dynamicfunction.htm";
hmContextIds["103"]="definitions_hook.htm";
hmContextIds["104"]="definitions_subhook.htm";
