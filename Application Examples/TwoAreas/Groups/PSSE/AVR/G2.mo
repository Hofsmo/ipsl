within TwoAreas.Groups.PSSE.AVR;
model G2
  extends iPSL.Electrical.Essentials.pfComponent;

  iPSL.Connectors.PwPin pwPin annotation (Placement(transformation(extent={{60,-10},{80,10}}), iconTransformation(extent={{60,-10},{80,10}})));
  iPSL.Electrical.Machines.PSSE.GENROU.GENROU g2(
    Tpd0=8,
    Tppd0=0.03,
    Tppq0=0.05,
    H=6.5,
    Xd=1.8,
    Xq=1.7,
    Xpd=0.3,
    Xppd=0.25,
    Xppq=0.25,
    Xl=0.2,
    R_a=0.0025,
    D=0.02,
    S12=0.802,
    S10=0.18600,
    M_b=900,
    V_b=V_b,
    V_0=V_0,
    angle_0=angle_0,
    P_0=P_0,
    Q_0=Q_0,
    Xpq=0.55,
    Tpq0=0.4) annotation (Placement(transformation(extent={{6,-18},{46,18}})));
  iPSL.Electrical.Controls.PSSE.ES.ESDC1A.ESDC1A eSDC1A(
    T_R=0.5,
    K_A=20,
    T_A=0.055,
    T_B=1,
    T_C=1,
    V_RMAX=4,
    V_RMIN=-4,
    K_E=-0.052927,
    T_E=0.36,
    K_F=0.125,
    T_F1=1.8,
    E_1=1,
    E_2=2,
    S_EE_1=0.0164,
    S_EE_2=0.0481) annotation (Placement(transformation(extent={{-38,-18},{-4,0}})));
  Modelica.Blocks.Sources.Constant const(k=0) annotation (Placement(transformation(extent={{-56,4},{-48,12}})));
equation

  connect(g2.PMECH, g2.PMECH0) annotation (Line(points={{6.4,9},{-2,9},{-2,32},{52,32},{52,-5.4},{47.6,-5.4}}, color={0,0,127}));
  connect(g2.p, pwPin) annotation (Line(points={{48,0},{44,0},{70,0}}, color={0,0,255}));
  connect(g2.EFD, eSDC1A.EFD) annotation (Line(points={{6.4,-9},{-3.05556,-9}}, color={0,0,127}));
  connect(eSDC1A.EFD0, g2.EFD0) annotation (Line(points={{-24.7778,-16.65},{-24.7778,-24},{56,-24},{56,-12.6},{47.6,-12.6}}, color={0,0,127}));
  connect(const.y, eSDC1A.VUEL) annotation (Line(points={{-47.6,8},{-42,8},{-42,-15.75},{-37.0556,-15.75}}, color={0,0,127}));
  connect(eSDC1A.VOEL, eSDC1A.VUEL) annotation (Line(points={{-37.0556,-11.25},{-42,-11.25},{-42,-15.75},{-37.0556,-15.75}}, color={0,0,127}));
  connect(eSDC1A.VOTHSG, eSDC1A.VUEL) annotation (Line(points={{-37.0556,-6.75},{-42,-6.75},{-42,-15.75},{-37.0556,-15.75}}, color={0,0,127}));
  connect(eSDC1A.ECOMP, eSDC1A.VUEL) annotation (Line(points={{-37.0556,-2.475},{-42,-2.475},{-42,-15.75},{-37.0556,-15.75}}, color={0,0,127}));
  annotation (
    Diagram(coordinateSystem(
        preserveAspectRatio=false,
        extent={{-60,-60},{60,60}},
        initialScale=0.1)),
    Icon(coordinateSystem(
        preserveAspectRatio=true,
        extent={{-60,-60},{60,60}},
        initialScale=0.1), graphics={
        Ellipse(extent={{-60,60},{60,-60}}, lineColor={28,108,200}),
        Line(points={{-40,0},{-20,20}}, color={28,108,200}),
        Line(points={{-20,20},{20,-20},{40,0}}, color={28,108,200}),
        Text(
          extent={{-20,-22},{16,-52}},
          lineColor={28,108,200},
          textString="G2")}),
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
<td>Klein-Rogers-Kundur power network</td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>2015-12-01</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Tin Rabuzin, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
end G2;
