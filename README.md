# PasSteam



**Based On**

Parts (anything with castle in) from current [CGE](https://castle-engine.io/) (Castle Game Engine) [repo](https://github.com/castle-engine/castle-engine)

Located in the steam folder you will find the following essential sources.

castledynlib.pas - this is a direct copy from CGE

castleinternalsteamapi.pas - this is the main Steam API translation

castlesteam.pas - this encapsulates Steam into a more friendly OO set of objects

**Overrides**

As this system is intended to complement and become part of CGE as well as function in a stand-alone library it is necessary to have stand-alone replacements for certain parts of CGE that are used, these are as follows which are all in steam/overrides 

castleapplicationproperties.pas

castlefilesutils.pas

castlelog.pas

castlestringutils.pas

castleutils.pas

Additionally castleconf.inc in the steam folder is a bespoke highly minimalistic version of a far larger one used by CGE proper.



**Purpose**

The intent is to make this a more up-to-date implementation of Steam Interface Pascal (Delphi/FPC+Lazarus) than is currently available (the widely used one is 14 years old and counting).

This library is intended to have as minimal a set of dependencies as possible while keeping it fully CGE compliant. 

As a result PasSteam should be capable of adding Steam support to any Delphi/FPC based application from simple form-based apps thru simple Skia-enabled games to fully fledged games utilising for example SDL. CGE will, hopefully, include it's own copy of PasSteam at some point.





