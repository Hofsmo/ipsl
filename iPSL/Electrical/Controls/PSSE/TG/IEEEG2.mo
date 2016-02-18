within iPSL.Electrical.Controls.PSSE.TG;
model IEEEG2
  import Modelica.SIunits.Time;

  parameter Real K=20 "Governor gain, 1/R (pu) generator MVA Base";
  parameter Time T1=0.5 "Governor lag time constant (s)";
  parameter Time T2=9 "Governor lead time constant (s)";
  parameter Time T3=77 "Gate actuator time constant (s)";
  parameter Time T4=2 "Water starting time (s)";
  parameter Real PMax=1 "Gate maximum (pu) machine MVA rating";
  parameter Real PMin=0 "Gate minimum (pu) machine MVA rating";
  parameter Real PMECH0 "Reference power (pu) machine MVA rating";

  Modelica.Blocks.Interfaces.RealInput SPEED annotation (Placement(transformation(extent={{-160,-20},{-120,20}}), iconTransformation(extent={{-160,-20},{-120,20}})));

  NonElectrical.Continuous.LeadLag leadLag(
    K=1,
    T1=T2,
    T2=T3,
    y_start=0) annotation (Placement(transformation(extent={{-46,-10},{-26,10}})));
  Modelica.Blocks.Nonlinear.Limiter limiter(uMax=PMax, uMin=PMin) annotation (Placement(transformation(extent={{16,-10},{36,10}})));
  Modelica.Blocks.Interfaces.RealOutput PMECH annotation (Placement(transformation(extent={{100,-16},{132,16}}), iconTransformation(extent={{100,-16},{132,16}})));
  NonElectrical.Continuous.LeadLag Hydraulics(
    K=1,
    T1=-T4,
    T2=T4/2,
    y_start=PMECH0) annotation (Placement(transformation(extent={{68,-10},{88,10}})));

  NonElectrical.Continuous.SimpleLag simpleLag(
    K=1,
    T=T1,
    y_start=0) annotation (Placement(transformation(extent={{-78,-10},{-58,10}})));
  Modelica.Blocks.Math.Gain gain(k=K) annotation (Placement(transformation(extent={{-108,-10},{-88,10}})));
equation
  connect(Hydraulics.u, limiter.y) annotation (Line(points={{66,0},{52,0},{37,0}}, color={0,0,127}));
  connect(PMECH, Hydraulics.y) annotation (Line(points={{116,0},{89,0}}, color={0,0,127}));
  connect(simpleLag.y, leadLag.u) annotation (Line(points={{-57,0},{-52.5,0},{-48,0}}, color={0,0,127}));
  connect(gain.u, SPEED) annotation (Line(points={{-110,0},{-140,0}}, color={0,0,127}));
  connect(simpleLag.u, gain.y) annotation (Line(points={{-80,0},{-87,0}}, color={0,0,127}));
  connect(leadLag.y, limiter.u) annotation (Line(points={{-25,0},{14,0}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-120,-60},{100,60}}), graphics={
        Rectangle(
          extent={{-140,80},{140,-100}},
          lineColor={28,108,200},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{50,28},{-56,-40}},
          lineColor={28,108,200},
          textString="IEEEG2"),
        Text(
          extent={{-206,60},{-156,36}},
          lineColor={28,108,200},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid,
          textString="SPEED"),
        Text(
          extent={{-206,-20},{-152,-44}},
          lineColor={28,108,200},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid,
          textString="PMECH0"),
        Text(
          extent={{164,18},{210,-4}},
          lineColor={28,108,200},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid,
          textString="PMECH")}), Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-120,-60},{100,60}}), graphics={Rectangle(
          extent={{-116,20},{-22,-20}},
          lineColor={28,108,200},
          pattern=LinePattern.Dash), Text(
          extent={{-70,28},{-46,18}},
          lineColor={28,108,200},
          pattern=LinePattern.Dash,
          textString="Governor",
          fontSize=10)}));
end IEEEG2;
