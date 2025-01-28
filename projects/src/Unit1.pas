unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.MaterialSources, FMX.Controls3D,
  FMX.Objects3D, FMX.Viewport3D;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Model3D1: TModel3D;
    Model3D1Mat71: TLightMaterialSource;
    Model3D1Mat01: TLightMaterialSource;
    Model3D1Mat11: TLightMaterialSource;
    Model3D1Mat21: TLightMaterialSource;
    Model3D1Mat51: TLightMaterialSource;
    Model3D1Mat41: TLightMaterialSource;
    Model3D1Mat61: TLightMaterialSource;
    Model3D1Mat31: TLightMaterialSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.
