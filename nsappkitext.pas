unit nsappkitext;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  CocoaAll, LCLType;

type
  NSAppearance = objcclass external (NSObject, NSCodingProtocol)
  private
    _name : NSString;
    _bundle : NSBundle;
    _private : Pointer;
    _reserved : id;
    _auxilary : id;
    {$ifdef CPU32}
    _extra : array [0..1] of id;
    {$endif}

  public
    procedure encodeWithCoder(aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder(aDecoder: NSCoder): id; message 'initWithCoder:';

    function name: NSString; message 'name';

    // Setting and identifying the current appearance in the thread.
    class function currentAppearance: NSAppearance; message 'currentAppearance';
    // nil is valid and indicates the default appearance.
    class procedure setCurrentAppearance(appearance: NSAppearance); message 'setCurrentAppearance:';

    // Finds and returns an NSAppearance based on the name.
    // For standard appearances such as NSAppearanceNameAqua, a built-in appearance is returned.
    // For other names, the main bundle is searched.
    class function appearanceNamed(aname: NSString): NSAppearance; message 'appearanceNamed:';

   {/* Creates an NSAppearance by searching the specified bundle for a file with the specified name (without path extension).
    If bundle is nil, the main bundle is assumed.
    */
    #if NS_APPEARANCE_DECLARES_DESIGNATED_INITIALIZERS
    - (nullable instancetype)initWithAppearanceNamed:(NSString *)name bundle:(nullable NSBundle *)bundle NS_DESIGNATED_INITIALIZER;
    - (nullable instancetype)initWithCoder:(NSCoder *)aDecoder NS_DESIGNATED_INITIALIZER;
    #endif}

    // Query allowsVibrancy to see if the given appearance actually needs vibrant drawing.
    // You may want to draw differently if the current apperance is vibrant.
    function allowsVibrancy: Boolean; message 'allowsVibrancy';
  end;
  procedure setThemeMode(FormHandle: HWND; isDarkMode: boolean);


var
  NSAppearanceNameAqua: NSString; cvar; external;
  // Light content should use the default Aqua apppearance.
  NSAppearanceNameLightContent: NSString; cvar; external; // deprecated

  // The following two Vibrant appearances should only be set on an NSVisualEffectView, or one of its container subviews.
  NSAppearanceNameVibrantDark : NSString; cvar; external;
  NSAppearanceNameVibrantLight: NSString; cvar; external;

type
  //it's actually a protocol!
  NSAppearanceCustomization = objccategory external (NSObject)
    procedure setAppearance(aappearance: NSAppearance); message 'setAppearance:';
    function appearance: NSAppearance; message 'appearance';

    // This returns the appearance that would be used when drawing the receiver, taking inherited appearances into account.
    //
    function effectiveAppearance: NSAppearance; message 'effectiveAppearance';
  end;


implementation

procedure setThemeMode(FormHandle: HWND; isDarkMode: boolean);
var
  theWindow : CocoaAll.NSWindow;
begin
  theWindow := NSView(FormHandle).window;
  if isDarkMode then
    theWindow.setAppearance (NSAppearance.appearanceNamed(NSAppearanceNameVibrantDark))
  else
    theWindow.setAppearance (NSAppearance.appearanceNamed(NSAppearanceNameAqua));
  theWindow.invalidateShadow;
  //window.invalidateShadow()

end;

(*{$IFDEF LCLCocoa}
{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$ENDIF}  *)

end.

