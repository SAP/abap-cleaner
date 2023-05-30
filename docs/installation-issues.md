# Known installation issues 

## Error "Unable to read repository ... PKIX path building failed"

**Symptom**: After copying the URL https://sap.github.io/abap-cleaner/updatesite into 
Eclipse (menu 'Help / Install New Software', field 'Work with'), the error "Unable to read repository at https://sap.github.io/abap-cleaner/updatesite/content.xml"
is shown with details "PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: 
unable to find valid certification path to requested target" (or similar). 

**Solution**: Open the file ```eclipse.ini``` from your Eclipse installation path. 
Right after the line ```-vmargs```, add the following two lines: 

```
-Djavax.net.ssl.trustStore=NONE
-Djavax.net.ssl.trustStoreType=Windows-ROOT
```

Now save ```eclipse.ini```, restart Eclipse and try menu 'Help / Install New Software' again.

**Explanation**: Some component in your network (antivirus, firewall, corporate proxy etc.) is hooking into encrypted 
TLS connections. The fake certificate of this component needs to be trusted by the operating system / application.
Typically, this is done by injecting a trusted root certificate into the Windows operating system trust store. 
However, Eclipse, in its default configuration, does not make use of the Windows OS trust store, but uses the trust store 
of the Java VM. The above solution makes Eclipse use the Windows OS trust store. 

## Info "No updates found" although a newer release exists

**Symptom**: Eclipse menu "Help / Check for Updates" reports "No updates found", although the 
[Release notes](release-notes.md) show that there is a newer ABAP cleaner release, and although Eclipse 
menu "Window / Preferences -\> Install/Update -\> Available Software Sites" shows the entry 
"ABAP Cleaner for ABAP Development Tools (ADT)". 

**Solution**: Same solution as above for 'Error "Unable to read repository ... PKIX path building failed"'.

## Error "An error occurred while collecting items to be installed"

**Symptom**: When installing the ABAP cleaner plug-in into Eclipse, the following error is shown: "Problem occurred: 
'Installing Software' has encountered a problem. An error occurred while collecting items to be installed".
Details say "No repository found containing: osgi.bundle,..." (or similar).

**Solution**: Please use a newer Eclipse release that is listed on https://tools.hana.ondemand.com/ as compatible for ABAP Development Tools. 

[**Back to README**](../README.md#requirements-and-installation)


