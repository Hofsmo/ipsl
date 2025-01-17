within TwoAreas;
model Two_Areas_PSAT

  import Modelica.Constants.pi;
  parameter Real r=0.0001;
  parameter Real x=0.001;
  parameter Real b=0.00175*0.5;
  iPSL.Electrical.Buses.Bus bus1 annotation (Placement(transformation(extent={{-190,20},{-170,40}})));
  iPSL.Electrical.Buses.Bus bus2 annotation (Placement(transformation(extent={{-190,-20},{-170,0}})));
  iPSL.Electrical.Buses.Bus bus3 annotation (Placement(transformation(extent={{170,20},{190,40}})));
  iPSL.Electrical.Buses.Bus bus4 annotation (Placement(transformation(extent={{170,-20},{190,0}})));
  iPSL.Electrical.Buses.Bus bus5 annotation (Placement(transformation(extent={{-150,20},{-130,40}})));
  iPSL.Electrical.Buses.Bus bus6 annotation (Placement(transformation(extent={{-110,20},{-90,40}})));
  iPSL.Electrical.Buses.Bus bus7 annotation (Placement(transformation(extent={{-70,20},{-50,40}})));
  iPSL.Electrical.Buses.Bus bus8 annotation (Placement(transformation(extent={{-10,20},{10,40}})));
  iPSL.Electrical.Buses.Bus bus9 annotation (Placement(transformation(extent={{50,20},{70,40}})));
  iPSL.Electrical.Buses.Bus bus10 annotation (Placement(transformation(extent={{90,20},{110,40}})));
  iPSL.Electrical.Buses.Bus bus11 annotation (Placement(transformation(extent={{130,20},{150,40}})));
  Groups.PSAT.G1 g1(
    P_0=700,
    Q_0=182.450226027067,
    V_0=1.03000000000000,
    angle_0=0.353381617759011*180/pi,
    V_b=20) annotation (Placement(transformation(extent={{-214,24},{-202,36}})));
  Groups.PSAT.G2 g2(
    P_0=700,
    Q_0=228.434194888742,
    V_0=1.01000000000000,
    angle_0=0.183079867930759*180/pi,
    V_b=20) annotation (Placement(transformation(extent={{-214,-16},{-202,-4}})));
  Groups.PSAT.G3 g3(
    Q_0=172.421333322957,
    P_0=718.906215107731,
    V_0=1.03000000000000,
    angle_0=-0.118682400000000*180/pi,
    V_b=20) annotation (Placement(transformation(
        extent={{-6,-6},{6,6}},
        rotation=180,
        origin={208,30})));
  Groups.PSAT.G4 g4(
    P_0=700,
    Q_0=193.556043309309,
    V_0=1.01000000000000,
    angle_0=-0.296314118513724*180/pi,
    V_b=20) annotation (Placement(transformation(
        extent={{-6,-6},{6,6}},
        rotation=180,
        origin={208,-10})));

  iPSL.Electrical.Branches.PwLine Line6_7(
    R=r*10,
    X=x*10,
    G=0,
    B=b*10) annotation (Placement(transformation(extent={{-90,20},{-70,40}})));
  iPSL.Electrical.Branches.PwLine Line5_6(
    R=r*25,
    X=x*25,
    G=0,
    B=b*25) annotation (Placement(transformation(extent={{-130,20},{-110,40}})));

  iPSL.Electrical.Branches.PwLine Line7_8_1(
    R=r*110,
    X=x*110,
    G=0,
    B=b*110/2) annotation (Placement(transformation(extent={{-40,30},{-20,50}})));
  iPSL.Electrical.Branches.PwLine Line7_8_2(
    R=r*110,
    X=x*110,
    G=0,
    B=b*110) annotation (Placement(transformation(extent={{-40,10},{-20,30}})));
  iPSL.Electrical.Branches.PwLine Line8_9_2(
    R=r*110,
    X=x*110,
    G=0,
    B=b*110) annotation (Placement(transformation(extent={{20,10},{40,30}})));
  iPSL.Electrical.Branches.PwLine Line8_9_1(
    R=r*110,
    X=x*110,
    G=0,
    B=b*110/2) annotation (Placement(transformation(extent={{20,30},{40,50}})));

  iPSL.Electrical.Branches.PwLine Line9_10(
    R=r*10,
    X=x*10,
    G=0,
    B=b*10) annotation (Placement(transformation(extent={{70,20},{90,40}})));

  iPSL.Electrical.Branches.PwLine Line10_11(
    R=r*25,
    X=x*25,
    G=0,
    B=b*25) annotation (Placement(transformation(extent={{110,20},{130,40}})));

  iPSL.Electrical.Events.PwFault pwFault(
    R=0,
    t1=1,
    t2=1.050,
    X=1e-5) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=270,
        origin={2,-16})));

  iPSL.Electrical.Loads.PSAT.ZIP Load7(
    P_0=967,
    Q_0=-100,
    Pz=0,
    Pi=1,
    Qz=0,
    Qi=0,
    V_0=0.962831011348560,
    angle_0=-0.0815909472281613*180/pi,
    V_b=230) annotation (Placement(transformation(extent={{-76,-30},{-52,-6}})));
  iPSL.Electrical.Loads.PSAT.ZIP Load9(
    P_0=1767,
    Q_0=-250,
    Pz=0,
    Pi=1,
    Qz=0,
    Qi=0,
    V_0=0.973864710338689,
    angle_0=-0.560256955944523*180/pi,
    V_b=230) annotation (Placement(transformation(extent={{80,-30},{54,-4}})));
  iPSL.Electrical.Branches.PSAT.TwoWindingTransformer trafo1(
    Sn=900,
    r=0,
    x=0.15,
    kT=20/230,
    V_b=20,
    Vn=20) annotation (Placement(transformation(extent={{-170,20},{-150,40}})));
  iPSL.Electrical.Branches.PSAT.TwoWindingTransformer trafo2(
    Sn=900,
    r=0,
    x=0.15,
    kT=20/230,
    V_b=20,
    Vn=20) annotation (Placement(transformation(extent={{-170,-20},{-150,0}})));
  iPSL.Electrical.Branches.PSAT.TwoWindingTransformer trafo3(
    Sn=900,
    r=0,
    x=0.15,
    kT=20/230,
    V_b=20,
    Vn=20) annotation (Placement(transformation(extent={{170,20},{150,40}})));
  iPSL.Electrical.Branches.PSAT.TwoWindingTransformer trafo4(
    Sn=900,
    r=0,
    x=0.15,
    kT=20/230,
    V_b=20,
    Vn=20) annotation (Placement(transformation(extent={{170,-20},{150,0}})));
  inner iPSL.Electrical.SystemBase SysData(fn=60) annotation (Placement(transformation(extent={{-220,46},{-170,60}})));
equation
  connect(g1.pwPin, bus1.p) annotation (Line(points={{-201.4,30},{-201.4,30},{-180,30}}, color={0,0,255}));
  connect(g2.pwPin, bus2.p) annotation (Line(points={{-201.4,-10},{-201.4,-10},{-180,-10}}, color={0,0,255}));
  connect(Line6_7.n, bus7.p) annotation (Line(points={{-73,30},{-66.5,30},{-60,30}}, color={0,0,255}));
  connect(Line6_7.p, bus6.p) annotation (Line(points={{-87,30},{-93.5,30},{-100,30}}, color={0,0,255}));
  connect(Line5_6.n, bus6.p) annotation (Line(points={{-113,30},{-106.5,30},{-100,30}}, color={0,0,255}));
  connect(Line5_6.p, bus5.p) annotation (Line(points={{-127,30},{-133.5,30},{-140,30}}, color={0,0,255}));
  connect(Line8_9_2.n, bus9.p) annotation (Line(points={{37,20},{54,20},{54,30},{60,30}}, color={0,0,255}));
  connect(Line8_9_1.n, bus9.p) annotation (Line(points={{37,40},{54,40},{54,30},{60,30}}, color={0,0,255}));
  connect(Line8_9_2.p, bus8.p) annotation (Line(points={{23,20},{6,20},{6,30},{0,30}}, color={0,0,255}));
  connect(Line8_9_1.p, bus8.p) annotation (Line(points={{23,40},{6,40},{6,30},{0,30}}, color={0,0,255}));
  connect(Line7_8_2.n, bus8.p) annotation (Line(points={{-23,20},{-6,20},{-6,22},{-6,30},{0,30}}, color={0,0,255}));
  connect(Line7_8_1.n, bus8.p) annotation (Line(points={{-23,40},{-6,40},{-6,30},{0,30}}, color={0,0,255}));
  connect(Line7_8_1.p, bus7.p) annotation (Line(points={{-37,40},{-54,40},{-54,30},{-60,30}}, color={0,0,255}));
  connect(Line7_8_2.p, bus7.p) annotation (Line(points={{-37,20},{-54,20},{-54,30},{-60,30}}, color={0,0,255}));
  connect(bus9.p, Line9_10.p) annotation (Line(points={{60,30},{73,30}}, color={0,0,255}));
  connect(Line9_10.n, bus10.p) annotation (Line(points={{87,30},{93.5,30},{100,30}}, color={0,0,255}));
  connect(bus10.p, Line10_11.p) annotation (Line(points={{100,30},{113,30}}, color={0,0,255}));
  connect(Line10_11.n, bus11.p) annotation (Line(points={{127,30},{133.5,30},{140,30}}, color={0,0,255}));
  connect(g4.pwPin, bus4.p) annotation (Line(points={{201.4,-10},{201.4,-10},{180,-10}}, color={0,0,255}));
  connect(g3.pwPin, bus3.p) annotation (Line(points={{201.4,30},{180,30}}, color={0,0,255}));
  connect(Load7.p, bus7.p) annotation (Line(points={{-64,-4.8},{-64,-4.8},{-64,0},{-64,28},{-64,30},{-60,30}}, color={0,0,255}));
  connect(Load9.p, Line9_10.p) annotation (Line(points={{67,-2.7},{67,-2.7},{67,30},{73,30}}, color={0,0,255}));
  connect(pwFault.p, bus8.p) annotation (Line(points={{2,-4.33333},{2,30},{0,30}}, color={0,0,255}));
  connect(bus1.p, trafo1.p) annotation (Line(points={{-180,30},{-171,30}}, color={0,0,255}));
  connect(bus5.p, trafo1.n) annotation (Line(points={{-140,30},{-149,30}}, color={0,0,255}));
  connect(bus2.p, trafo2.p) annotation (Line(points={{-180,-10},{-171,-10}}, color={0,0,255}));
  connect(trafo2.n, bus6.p) annotation (Line(points={{-149,-10},{-104,-10},{-104,30},{-100,30}}, color={0,0,255}));
  connect(trafo4.n, bus10.p) annotation (Line(points={{149,-10},{108,-10},{108,30},{100,30}}, color={0,0,255}));
  connect(trafo4.p, bus4.p) annotation (Line(points={{171,-10},{176,-10},{180,-10}}, color={0,0,255}));
  connect(trafo3.p, bus3.p) annotation (Line(points={{171,30},{175.5,30},{180,30}}, color={0,0,255}));
  connect(bus11.p, trafo3.n) annotation (Line(points={{140,30},{149,30}}, color={0,0,255}));
  annotation (
    Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-220,-40},{220,60}}), graphics={Text(
          extent={{-68,-34},{46,-38}},
          lineColor={28,108,200},
          textString="Two-Area System
Prabha Kundur, \"Power System Stability and Control\", Example 12.6, page 813")}),
    Icon(coordinateSystem(extent={{-220,-40},{220,60}})),
    Documentation(revisions="<html>
<!--DISCLAIMER-->
<p>Copyright 2015-2016 RTE (France), SmarTS Lab (Sweden), AIA (Spain) and DTU (Denmark)</p>
<ul>
<li>RTE: <a href=\"http://www.rte-france.com\">http://www.rte-france.com</a></li>
<li>SmarTS Lab, research group at KTH: <a href=\"https://www.kth.se/en\">https://www.kth.se/en</a></li>
<li>AIA: <a href=\"http://www.aia.es/en/energy\"> http://www.aia.es/en/energy</a></li>
<li>DTU: <a href=\"http://www.dtu.dk/english\"> http://www.dtu.dk/english</a></li>
</ul>
<p>The authors can be contacted by email: <a href=\"mailto:info@itesla-ipsl.org\">info@itesla-ipsl.org</a></p>

<p>This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. </p>
<p>If a copy of the MPL was not distributed with this file, You can obtain one at <a href=\"http://mozilla.org/MPL/2.0/\"> http://mozilla.org/MPL/2.0</a>.</p>
</html>", info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>Two Area System PSAT, d_kundur1.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"),
    experiment(
      StopTime=10,
      Interval=0.0001,
      Tolerance=1e-006,
      __Dymola_fixedstepsize=0.0001,
      __Dymola_Algorithm="Rkfix2"),
    __Dymola_experimentSetupOutput);
end Two_Areas_PSAT;
