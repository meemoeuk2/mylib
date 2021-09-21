Global co_image_list:TList=New TList
Global polygon_list:TList=New TList
Global edge_list:TList=New TList
Global edge_list_list:TList=New TList
Global vertex_list:TList=New TList
'Include "C:\program files\blitztemp\mylibrary\myMathsLib.bb"
'Include "C:\users\james\documents\jamesblitz\mylibrary\mymathslib.bb"
Include "h:\myotherdoc\jamesblitz\mylibrary\mymathslib.bmx"

Type bbvertex Extends TBBType

	Method New()
		Add(vertex_list)
	End Method

	Method After:bbvertex()
		Local t:TLink
		t=_link.NextLink()
		If t Return bbvertex(t.Value())
	End Method

	Method Before:bbvertex()
		Local t:TLink
		t=_link.PrevLink()
		If t Return bbvertex(t.Value())
	End Method

Field x#,y#,el:bbedge_list
End Type

Type bbedge_list Extends TBBType

	Method New()
		Add(edge_list_list)
	End Method

	Method After:bbedge_list()
		Local t:TLink
		t=_link.NextLink()
		If t Return bbedge_list(t.Value())
	End Method

	Method Before:bbedge_list()
		Local t:TLink
		t=_link.PrevLink()
		If t Return bbedge_list(t.Value())
	End Method

Field e:bbedge,el:bbedge_list
End Type

Type bbedge Extends TBBType

	Method New()
		Add(edge_list)
	End Method

	Method After:bbedge()
		Local t:TLink
		t=_link.NextLink()
		If t Return bbedge(t.Value())
	End Method

	Method Before:bbedge()
		Local t:TLink
		t=_link.PrevLink()
		If t Return bbedge(t.Value())
	End Method

Field v1:bbvertex,v2:bbvertex
End Type

Type bbpolygon Extends TBBType' a list tof vertex

	Method New()
		Add(polygon_list)
	End Method

	Method After:bbpolygon()
		Local t:TLink
		t=_link.NextLink()
		If t Return bbpolygon(t.Value())
	End Method

	Method Before:bbpolygon()
		Local t:TLink
		t=_link.PrevLink()
		If t Return bbpolygon(t.Value())
	End Method

Field v:bbvertex,p:bbpolygon
End Type

Type bbco_image Extends TBBType

	Method New()
		Add(co_image_list)
	End Method

	Method After:bbco_image()
		Local t:TLink
		t=_link.NextLink()
		If t Return bbco_image(t.Value())
	End Method

	Method Before:bbco_image()
		Local t:TLink
		t=_link.PrevLink()
		If t Return bbco_image(t.Value())
	End Method

Field x#,y#,image,siloette,image2,xv#,yv#,xa#,ya#,active,xf#,yf#,id,counter,state,anim
Field start_timer,retire_time,flag
End Type
' counter is used with reverse frag pa. So that pat knows when to stop
' someimes the co_image changes is motion\behaviour
' siloette is a 2ndary image which represents the siloette of the image, Natch.
' start_timer co_image activates when this =0
' retire_time, this is start_time + move_timer (i.e. counter), and is the time when co_image
'	draws to the back. Used to decide in which order to draw co_images
' flag ; miscenious flag, used for sorting

' poly.bb
' by simon@acid.co.nz

Global xval[20+1]
Global yval[20+1]
Global trigbank_max=9104
Global cosbank#[trigbank_max+1]
Global sinbank#[trigbank_max+1]
create_trig_bank()
Global colbank[1024+1,768+1]' for fast draw of circle

'eg_triangle()
'Flip
'eg_quad2
'eg_quad
'eg_n_poly()

Function eg_quad()

Graphics 640,480
SetColor 222,222,222
quad(320,240,500,240,600,240,200,400)

End Function



Function edges_cross(e1:bbedge,e2:bbedge)

If e1.v1=e2.v1 Or e1.v1=e2.v2 Or e1.v2=e2.v1 Or e1.v2=e2.v2 Then Return
If Not bipolar_vertexs(e1.v1,e1.v2,e2) Then Return
If Not bipolar_vertexs(e2.v1,e2.v2,e1) Then Return

Return 1'ec

End Function



Function pti_check(clean) ' polygon typed triangle list integrity check
' clean , pti will remove none-triangle polygon objects, else just returns 1 if all polys are tris

p:bbpolygon=bbpolygon(polygon_list.First())
While p<>Null
 z=0
 If Not is_triangle(p) 
  If Not clean Then Return 0

  ' delete
  If p<>Null
   If p.p<>Null
    If p.p.p<>Null Then p.p.p.Remove() ' else
    p.p.Remove()
   EndIf
   p.Remove()
   pp:bbpolygon=p.After()
   z=1
  EndIf

 EndIf
 If z=0
  For i=0 To 2
   If p=Null Then DebugLog " bizzare! in pticheck!";Return 0 ' instances must come in 3's otherwise, not a proper triangle list.
   p=p.After()
  Next
 Else
  p=pp
 EndIf
Wend 

Return 1

End Function



Function bipolar_vertexs(v:bbvertex,v2:bbvertex,e:bbedge)

ve1=v_to_e_polarity(v,e)
ve2=v_to_e_polarity(v2,e)
If ve1=-ve2 And ve1<>0 Then Return 1

End Function



Function create_edge(v1:bbvertex,v2:bbvertex)

e:bbedge=New bbedge
e.v1=v1
e.v2=v2

el1:bbedge_list=e.v1.el
While el1.e<>Null
 el1=el1.el
Wend
If el1.e=Null
 If el1.el<>Null Then DebugStop ' *** ! if edge list exists then edge vertexs must exist!
 el1.e=e
 el1.el=New bbedge_list
Else 
 Return 0
EndIf

el1:bbedge_list=e.v2.el
While el1.e<>Null
 el1=el1.el
Wend
If el1.e=Null 
 If el1.el<>Null Then DebugStop
 el1.e=e
 el1.el=New bbedge_list 
Else 
 Return 0
EndIf

'Line e\v1\x,e\v1\y,e\v2\x,e\v2\y

End Function



Function vertexs_are_connected(v1:bbvertex,v2:bbvertex)

el1:bbedge_list=v1.el
If el1=Null Then Return 0

While el1.e<>Null 
 If el1.e.v1=v2 Or el1.e.v2=v2 Then Return 1
 el1=el1.el
Wend

End Function



Function is_triangle(p:bbpolygon)

If p<>Null
 If p.p<>Null
  If p.p.p<>Null
   If vertexs_are_connected(p.v,p.p.v) 
    If vertexs_are_connected(p.v,p.p.p.v)
     If vertexs_are_connected(p.p.v,p.p.p.v)
      Return 1
     EndIf
    EndIf
   EndIf
  EndIf
 EndIf
EndIf

End Function 



Function get_converge_vertex:bbvertex(e1:bbedge,e2:bbedge)

m1#=(e1.v2.y-e1.v1.y)/(e1.v2.x-e1.v1.x)
m2#=(e2.v2.y-e2.v1.y)/(e2.v2.x-e2.v1.x)
c1#=e1.v1.y-e1.v1.x*m1
c2#=e2.v1.y-e2.v1.x*m2
xc#=(c2-c1)/(m1-m2)
yc#=c1+xc*m1
v:bbvertex=New bbvertex
v.x=xc
v.y=yc
Return v

End Function



Function vertex_is_on_edge(v:bbvertex,e:bbedge)

a#=0.01 ' accuracy
m2#=(e.v1.y-e.v2.y)/(e.v1.x-e.v2.x)
c#=e.v1.y-e.v1.x*m2
If v.x*m2+c-v.y<a Then Return 1

End Function



Function crossed_edge(e:bbedge)

For f:bbedge=EachIn edge_list
 If e<>f
  If edges_cross(e,f) Then Return 1
 EndIf
Next

End Function

'v_e_polarity_exe()
Function v_e_polarity_exe()

v:bbvertex=New bbvertex
v.x=200
v.y=200
e:bbedge=New bbedge
ve2:bbvertex=New bbvertex
ve2.x=150
ve2.y=50
ve:bbvertex=New bbvertex
ve.x=100
ve.y=100

e.v1=ve
e.v2=ve2
Graphics 640,480
While Not VKeyDown(1)

If MouseHit(1) Then b=1-b
If b
ve2.x=MouseX()
ve2.y=MouseY()
Else 
ve.x=MouseX()
ve.y=MouseY()
EndIf

SetColor 222,222,222
DrawLine e.v1.x,e.v1.y,e.v2.x,e.v2.y
Plot v.x,v.y
DrawText v_to_e_polarity(v,e),400,400

Cls

Wend

End Function



Function eg_quad2()

Graphics 640,480

SetColor 222,222,222
z=Sqr(320*320+240*240)
While Not VKeyDown(1)
 a#=a+0.1
 h=0
 Cls
 While h<5
  x1=320+z*Cos(a+h*72)
  y1=240+z*Sin(a+h*72)
  x2=320+z*Cos(a+(h+1)*72)
  y2=240+z*Sin(a+(h+1)*72)
  SetColor 222,222,50*h+40
  Plot 320,240
  Plot x1,y1
  Plot x2,y2
  Plot x1+x2-320,y1+y2-240
  
  quad(320,240,x1,y1,x1+x2-320,y1+y2-240,x2,y2)
  h=h+1
 Wend
 Flip 0
Wend
End

End Function



Function eg_triangle()

Graphics 1024,768
SetColor 50,50,150
triangle(50,50,100,100,50,100)
Flip
WaitMouse()
End

End Function



Function Triangle(x0,y0,x1,y1,x2,y2)

p:bbpolygon=New bbpolygon
p.v=New bbvertex
p.v.x=x0
p.v.y=y0
p.p=New bbpolygon
p.p.v=New bbvertex
p.p.v.x=x1
p.p.v.y=y1
p.p.p=New bbpolygon
p.p.p.v=New bbvertex
p.p.p.v.x=x2
p.p.p.v.y=y2

order_triangle_cw(p)
n_poly(p,0,0)

End Function



Function Quad(x0,y0,x1,y1,x2,y2,x3,y3)
xval(0)=x0
yval(0)=y0
xval(1)=x1
yval(1)=y1
xval(2)=x2
yval(2)=y2
xval(3)=x3
yval(3)=y3
poly(4)
End Function



Function eg_n_poly()

Graphics 640,480
SetColor 222,222,222
p:bbpolygon=New bbpolygon
p.v=New bbvertex
p.v.x=50
p.v.y=100
p.p=New bbpolygon
p.p.v=New bbvertex
p.p.v.x=51
p.p.v.y=51
p.p.p=New bbpolygon
p.p.p.v=New bbvertex
p.p.p.v.x=102
p.p.p.v.y=102

order_triangle_cw(p)
n_poly(p,0,0)

End Function


Function n_poly(p:bbpolygon,x,y) ' x,y are offsets
' *** this probably won't work if vertices aren't orederd '
' clockwise ordering works

While p<>Null
 xval(i)=p.v.x-x
 yval(i)=p.v.y-y
 i=i+1
 p=p.p
Wend
If i>2 Then poly(i)

End Function



Function v_to_e_polarity(v:bbvertex,e:bbedge)

xp=v.x-e.v1.x
yp=v.y-e.v1.y
xe#=e.v2.x-e.v1.x
ye#=e.v2.y-e.v1.y
If (e.v2.x-e.v1.x)<>0
 If e.v2.x-e.v1.x<0 Then pp=-1 Else pp=1
 m2#=ye/xe
 p#=m2*xp-yp
ElseIf (e.v2.y-e.v1.y)<>0
 If e.v2.y-e.v1.y<0 Then pp=1 Else pp=-1
 im#=xe/ye
 p#=im*yp-xp
Else
 'DebugLog "v_to_e_polarity: duplicate vertex error"
 ' vertex is on edge line.
 Return 0
EndIf

Return Sgn(p)*pp

End Function



Function order_triangle_cw(p:bbpolygon) ' clock-wise
' simple in theory. First 2 vertexs make an edge, 3rd vertex must be a set sgn. pos, neg? which one?
a1=coord_to_angle(p.p.v.x-p.v.x,p.p.v.y-p.v.y)
a2=coord_to_angle(p.p.p.v.x-p.p.v.x,p.p.p.v.y-p.p.v.y)
a=mod2(a2-a1,360) 
If a>180 
 pp:bbvertex=p.p.p.v
 p.p.p.v=p.p.v
 p.p.v=pp
EndIf 

End Function



Function poly(vcount)
' get clipping region
width=GraphicsWidth()
height=GraphicsHeight()
' find top verticy
b=vcount-1
y=yval(0)
While c<>b
c=c+1
yy=yval(c)
If yy<y y=yy d=c
Wend
c=d 
t=c
' draw top to bottom
While y<height
' get left gradient
If y=yval(c)
While y=yval(c)
x0=xval(c) Shl 16
c=c+1
If c>b c=a
If c=t Return
If y>yval(c) Return
Wend
h=yval(c)-y
g0=((xval(c) Shl 16)-x0)/h
EndIf
' get right gradient
If y=yval(d)
While y=yval(d)
x1=xval(d) Shl 16
d=d-1
If d<a d=b
If y>yval(d) Return
Wend
h=yval(d)-y
g1=((xval(d) Shl 16)-x1)/h
EndIf
' calc horizontal span
x=x1 Sar 16
w=((x0 Sar 16)-x)+1
' draw down to next vert
If (w>0 And y>-1 And x<width And x+w>0)
If x<0 w=w+x x=0 'crop left
If x+w>width w=width-x 'crop right
DrawRect x,y,w,1
EndIf
' next 
x0=x0+g0
x1=x1+g1
y=y+1
Wend

End Function


'eg_drawbox()
Function eg_drawbox()

Graphics 640,480

SetColor 222,222,222
drawbox(50,50,100,200,10)
Flip
WaitMouse()
End

End Function



Function drawbox(x,y,xs,ys,width)

DrawRect x,y,xs,width
DrawRect x,y+ys-width,xs,width
DrawRect x,y+width,width,ys-2*width
DrawRect x+xs-width,y+width,width,ys-2*width

End Function



Function edge_inside_triangle(e:bbedge,p:bbpolygon)
' special. for edges with onee vertex in common polygon.


' with other sesparate vertex, create an edge connecting to each vertx of the triangle
' if one of these edges crossed an edge of te triangle then it's not insode triangle.
' first, find edge's and trinagles common vertex
nc:bbvertex=Null
If vertex_common_triangle(e.v1,p) Then nc=e.v2 Else nc=e.v1
' first check if inside bounding box
xmin=9999
xmax=-9999
ymin=9999
ymax=-9999

While t2:bbpolygon<>Null
 If t2.v.x<xmin Then xmin=t2.v.x
 If t2.v.x>xmax Then xmax=t2.v.x
 If t2.v.y<ymin Then ymin=t2.v.y
 If t2.v.y>ymax Then ymax=t2.v.y
 t2=t2.p
Wend

If xmax<=xmin Or ymax<=ymin Then DebugStop;Return ' failed to make valid bounding box

If Not (nc.x>=xmin And nc.x<xmax And nc.y>=ymin And nc.y<=ymax) Then Return 0

' now create edge with vertex and and non common triangle vertex
' and check if edges cross
e2:bbedge=New bbedge
'e2\


' same with other non commnon t v

' if no crossing then   
End Function


'vit_exe()
Function vit_exe()

Graphics 640,480

v1:bbvertex=New bbvertex
v1.x=100
v1.y=100
v2:bbvertex=New bbvertex
v2.x=150
v2.y=100
v3:bbvertex=New bbvertex
v3.x=150
v3.y=150
v4:bbvertex=New bbvertex

p:bbpolygon=New bbpolygon
p.v=v1
t2:bbpolygon=New bbpolygon
p.p=t2
p.p.v=v2
t3:bbpolygon=New bbpolygon
p.p.p=t3
t3.v=v3

SetColor 222,222,222
b=3
While Not VKeyDown(1)
 DrawLine v1.x,v1.y,v2.x,v2.y
 DrawLine v2.x,v2.y,v3.x,v3.y
 DrawLine v3.x,v3.y,v1.x,v1.y
 Plot v4.x,v4.y 

 Select b
  Case 0;v1.x=MouseX();v1.y=MouseY()
  Case 1;v2.x=MouseX();v2.y=MouseY()
  Case 2;v3.x=MouseX();v3.y=MouseY()
  Case 3;v4.x=MouseX();v4.y=MouseY()
 End Select

 If MouseHit(1) Then b=(b+1) Mod 4

 DrawText 400,400,vertex_inside_triangle(v4,p)
 Flip
 Cls
Wend
End Function



Function vertex_inside_triangle(v:bbvertex,p:bbpolygon)
' excluding boundary

a#=get_angle(p.v,v,p.p.v)+get_angle(p.p.v,v,p.p.p.v)+get_angle(p.p.p.v,v,p.v)
If a>359.8 And a<360.2 Then Return 1 ' rough estimate

End Function



Function vertex_common_triangle(v:bbvertex,p:bbpolygon)

If p.v=v Then Return 1
If p.p.v=v Then Return 1
If p.p.p.v=v Then Return 1

End Function



Function get_angle#(v1:bbvertex,v2:bbvertex,v3:bbvertex)
' the angle made by 3 vertexs, at 2nd vertex

'get vectors
abx#=v1.x-v2.x
aby#=v1.y-v2.y
bcx#=v3.x-v2.x
bcy#=v3.y-v2.y
' get norms
abn#=Sqr(abx*abx+aby*aby)
bcn#=Sqr(bcx*bcx+bcy*bcy)
' get dot product
dp#=abx*bcx+aby*bcy

a#=ACos(dp/(abn*bcn))

Return a

End Function



Function eg_get_Angle()

v1:bbvertex=New bbvertex
v1.x=100
v1.y=100
v2:bbvertex=New bbvertex
v2.x=150
v2.y=100
v3:bbvertex=New bbvertex
v3.x=50
v3.y=50

Print get_angle(v1,v2,v3)

End Function



Function create_trig_bank()

While i#<360
 cosbank(p)=Cos(i)
 sinbank(p)=Sin(i)
 i#=i+0.03954
 p=p+1
Wend

End Function



Function my_circle(x#,y#,r#,col,fast)

num#=3*r*Pi
pn#=Float(trigbank_max)/num
gw#=Float(GraphicsWidth())
gh#=Float(GraphicsHeight())

While pp#<trigbank_max
 p=Floor(pp)
 xx#=x+r*cosbank(p)
 yy#=y+r*sinbank(p)
 If xx>=0 And xx<gw And yy>=0 And yy<gh
  If xx>639.0 Then xx=639.0
  If yy>479.0 Then yy=479.0
  Plot xx,yy'If fast Then WritePixel xx,yy,col Else WritePixel xx,yy,col
 EndIf
 pp#=pp+pn
Wend

End Function



Function my_circle2(x#,y#,r#,fast)

t=MilliSecs()

num#=3*r*Pi
pn#=Float(trigbank_max)/num
gw#=Float(GraphicsWidth())
gh#=Float(GraphicsHeight())

While pp#<trigbank_max
 p=Floor(pp)
 xx#=x+r*cosbank(p)
 yy#=y+r*sinbank(p)
 xx2=Int(xx)
 yy2=Int(yy)
 If xx>=0 And xx<gw And yy>=0 And yy<gh
  col=colbank(Int(xx),Int(yy))
  If xx>639.0 Then xx=639.0
  If yy>479.0 Then yy=479.0
'  If fast Then WritePixelFast xx,yy,col Else dev.blitz3d.WritePixel xx,yy,col
  Plot xx,yy
 EndIf
 pp#=pp+pn
Wend
'debuglog (MilliSecs()-t)+" for my_circle2 radius "+r

End Function



Function get_colbank(xmin,ymin,xmax,ymax)

For y=ymin To ymax
For x=xmin To xmax
' colbank(x,y)=ReadPixel(x,y)
Next
Next

End Function


'eg_circle()
Function eg_circle()

Graphics 1024,768
t=MilliSecs()
While MilliSecs()-20<t
my_circle(512,384,1,-34574,0)
'Rect 0,0,1024,768,0
'Oval 0,0,768,768,0
i=i+1
Wend
Print i
WaitMouse()
End

End Function



Function vector_rect_hit(vx#,vy#,vxv#,vyv#,rectx#,recty#,rectw#,recth#)
' cheapo-quick version. Properly, should be done with vector functions developed
' for random pologon shatter. But this should be ok for morph.

' already in
If vx>rectx And vx<rectx+rectw
 If vy>recty And vy<recty+recth
  Return 1
 EndIf
EndIf

If vxv=0 And vyv=0 Then Return 0

' get direction and distance of rect in components, relative to vector point
VR_TL_x=rectx-vx
VR_TL_y=recty-vy
VR_BR_x=rectx+rectw-vx
VR_BR_y=recty+recth-vy

' is vector going in right fdirection?
' need case for zero velcity.

direction_ok_TL=1
direction_ok_BR=1
If Sgn(vxv)<>Sgn(VR_TL_X) And vxv<>0 Then direction_ok_TL=0
If Sgn(vyv)<>Sgn(VR_TL_Y) And vyv<>0 Then direction_ok_TL=0
If Sgn(vxv)<>Sgn(VR_BR_X) And xvx<>0 Then direction_ok_BR=0
If Sgn(vyv)<>Sgn(VR_BR_Y) And vyv<>0 Then direction_ok_BR=0

If vxv<>0 And vyv<>0
 direction_ok=direction_ok_TL | direction_ok_BR
Else
 direction_ok=direction_ok_TL & direction_ok_BR
EndIf

Return direction_ok

End Function


'eg_rotate_poly()
Function eg_rotate_poly()

Graphics 640,480
SetColor 222,222,222
p:bbpolygon=New bbpolygon
p.v=New bbvertex
p.v.x=100
p.v.y=100
p.p=New bbpolygon
p.p.v=New bbvertex
p.p.v.x=200
p.p.v.y=100
p.p.p=New bbpolygon
p.p.p.v=New bbvertex
p.p.p.v.x=200
p.p.p.v.y=200
p.p.p.p=New bbpolygon
p.p.p.p.v=New bbvertex
p.p.p.p.v.x=100
p.p.p.p.v.y=200

display_poly(p)
WaitMouse()
orig:bbvertex=New bbvertex
orig.x=150
orig.y=150
rotate_poly(orig,p,45)
display_poly(p)
WaitMouse()
End

End Function


Function rotate_poly(orig:bbvertex,p:bbpolygon,angle#)

While p<>Null
 x#=p.v.x-orig.x
 y#=p.v.y-orig.y
 r#=Sqr(x*x+y*y)
 ang#=coord_to_angle(x,y)+angle
 p.v.x=orig.x+r*Cos(ang)
 p.v.y=orig.y+r*Sin(ang)
 p=p.p
Wend

End Function



Function display_poly(p:bbpolygon)

pol2:bbpolygon=p

While p<>Null
 If p.p=Null Then Exit
 DrawLine p.v.x,p.v.y,p.p.v.x,p.p.v.y
 p=p.p
Wend
If p<>Null Then DrawLine p.v.x,p.v.y,pol2.v.x,pol2.v.y

End Function
