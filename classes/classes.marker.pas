unit classes.marker;

interface

uses uGlobal, System.Sensors;

type
  TMarker = class
  private
    FMarkerType: integer;
    FCoords: TLocationCoord2D;

  published
    property MarkerType: integer read FMarkerType write FMarkerType;
    property Coords: TLocationCoord2D read FCoords write FCoords;
  end;

implementation

end.
