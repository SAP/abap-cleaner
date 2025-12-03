# Known installation issues 

## Error "Unable to read repository ... PKIX path building failed"

**Symptom**: After copying the URL https://sap.github.io/abap-cleaner/updatesite into 
Eclipse (menu 'Help / Install New Software', field 'Work with'), the error "Unable to read repository at https://sap.github.io/abap-cleaner/updatesite/content.xml"
is shown with details "PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: 
unable to find valid certification path to requested target" (or similar). 

**Solution**: Use Eclipse 2025-12 or later.

In case you still encounter issues, make sure that there are no unwanted modifications to ```eclipse.ini``` from your Eclipse installation path.

The following option below ```-vmargs``` **should be set** (Eclipse 2025-12 default):
```
-Declipse.platform.mergeTrust=true
```
The following options must **not** be set anymore:
```
-Djavax.net.ssl.trustStore=NONE
-Djavax.net.ssl.trustStoreType=Windows-ROOT
```

Now save ```eclipse.ini```, restart Eclipse and try menu 'Help / Install New Software' again.

**Explanation**: Some component in your network (antivirus, firewall, corporate proxy etc.) is hooking into encrypted 
TLS connections. The fake certificate of this component needs to be trusted by the operating system / application.
Typically, this is done by injecting a trusted root certificate into the Windows operating system trust store. 
However, Eclipse, in its default configuration:
- before Eclipse 2024-12, did not make use of the Windows OS trust store, but only used the trust store
  of the Java VM.
- starting with Eclipse 2024-12, used the Windows OS trust store only and not the JVM store anymore.
  However, the OS store might not necessarily be filled properly, also resulting in problems.
- starting with Eclipse 2025-12, combines the OS and JVM trust stores - also on macOS.

For more background information, see https://eclipse.dev/eclipse/markdown/?f=news/4.38/platform.md#merging-the-jvm-and-the-operating-system-trust-stores


## Info "No updates found" although a newer release exists

**Symptom**: Eclipse menu "Help / Check for Updates" reports "No updates found", although the 
[Release notes](release-notes.md) show that there is a newer ABAP cleaner release, and although Eclipse 
menu "Window / Preferences -\> Install/Update -\> Available Software Sites" shows the entry 
"ABAP Cleaner for ABAP Development Tools (ADT)". 

**Solution**: Same solution as above for 'Error "Unable to read repository ... PKIX path building failed"'.


## Error "not reachable", although updatesite URL is correct

**Symptom**: Eclipse menu "Help / Check for Updates" says that the URL of the ABAP cleaner updatesite 
https://sap.github.io/abap-cleaner/updatesite is not reachable, although the correct URL was entered. 

**Solution**: Same solution as above for 'Error "Unable to read repository ... PKIX path building failed"'.

**Note**: In a browser, the URL https://sap.github.io/abap-cleaner/updatesite will correctly be shown as 
"404 File not found", because this URL is a folder, not a file. To convince yourself that the updatesite 
can indeed be reached, you may open in your browser:
* https://sap.github.io/abap-cleaner, which redirects to the ABAP cleaner repository, or 
* https://sap.github.io/abap-cleaner/updatesite/p2.index, which downloads a small index file that is used by Eclipse. 


## Error "An error occurred while collecting items to be installed"

**Symptom**: When installing the ABAP cleaner plug-in into Eclipse, the following error is shown: "Problem occurred: 
'Installing Software' has encountered a problem. An error occurred while collecting items to be installed".
Details say "No repository found containing: osgi.bundle,..." (or similar).

**Solution**: Please use a newer Eclipse release that is listed on https://tools.hana.ondemand.com/ as compatible for ABAP Development Tools. 

[**Back to README**](../README.md#requirements-and-installation)


## Menu "Help / Install New Software ..." is grayed out

**Symptom**: In the SAP-internal version of ABAP Development Tools (ADT) installed from Software Center, 
the ABAP cleaner plug-in cannot be installed, because the ADT menu "Help / Install New Software ..." is grayed out. 

**Solution**: Please change to an extensible ADT version as described in SAP Note 3415582. 

[**Back to README**](../README.md#requirements-and-installation)
