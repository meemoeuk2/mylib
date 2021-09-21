'Import "bbtype.bmx"
'Import "bbvkey.bmx"

Global matrix_list:TList=New TList
Global column_list:TList=New TList
Global bit_bank[10]

Function fSin#(x#) ' a modest approx to sin, fast. requires radian input between -pi,+pi

If (x < 0.0)
 Return 1.27323954 * x + .405284735 * x * x'
Else
 Return 1.27323954 * x - 0.405284735 * x * x'
EndIf

End Function



Function fcos#(x#)

x=x+Pi/2
If (x < 0.0)
 Return 1.27323954 * x + 0.405284735 * x * x
Else
 Return 1.27323954 * x - 0.405284735 * x * x'
EndIf

End Function



Function fmax(a,b)
If a>b Then Return a Else Return b
End Function


Function fmin(a,b)
If a<b Then Return a Else Return b
End Function

'Print remainder_f(310,30)
Function remainder_f#(a#,d#) ' returnss hte remainder of 
' this function is quite inaccurate and should not be used for recursion.
' note thiss is the floating point number version of mod
p#=a/d
p=p-Floor(p)
p=p*d
Return p

End Function 



Function mod2(a,b)  ' no break in symetry when negative numbers are in the domain
					' e.g. -2 mod 3 = -2 , -2 mod2 3 = 1.			
If a<0 And b>0
 a=-a
 a=a Mod b
 a=b-a
ElseIf a>=0 And b<0
 b=-b
 a=a Mod b
 a=b-a
ElseIf a<0 And b<0
 b=-b
 a=-a
EndIf

If b=0 Then Return 0
Return a Mod b

End Function



Function mod2f#(a#,b#) ' same as mod2 but with floats

If a<0 And b>0
 a=-a
 a=a Mod b
 a=b-a
ElseIf a>=0 And b<0
 b=-b
 a=a Mod b
 a=b-a
ElseIf a<0 And b<0
 b=-b
 a=-a
EndIf

Return a Mod b

End Function 


' discrete chop ; complimentary function could discrete round
Function discrete(num,div)  ' assigns num to a discrete set. Don't know offical techinical term
							' e.g. num div 5 , 1..4 => 0 , 5..9 => 5 , 10..14=> 10 etc... 
Return num-mod2(num,div)

End Function


'Print round(3.499)
Function round_to_int(num#)

a=Int(num)
If num>=a+0.5 Then Return a+1 
Return a

End Function



Function round_to_nearest(s#,p) ' subject aand parameter , rounds down

If p=0 Then Return 0 
d=s/p
Return d*p

End Function



Function mod_dif(a,b,m)
' faster version of mod_dif, signed
c=(b-a) Mod m
d=c-m*Sgn(c)
If Abs(d)<=Abs(c) Then c=d
Return c

End Function



Function modf_dif#(a#,b#,m#)

c#=(b-a) Mod m
d#=c-m*Sgn(c)
If Abs(d)<=Abs(c) Then c=d
Return c

End Function

'Print mod_dif(2.3,9.2,10.0) ; old version of mod_dif, not used
'Function mod_dif(a,b,m)
' least distance from a to b
' seems to assume a and b are positive
'a=Mod2(a,m)
'b=Mod2(b,m)
'c=min(Abs(b-a),Abs(m+b-a))
'Return min(c,Abs(m+a-b))*mod_sgn(a,b,m)

'End Function



Function mod_sgn(a,b,m)
' direction of least distance from a to b
a=a Mod m
b=b Mod m
If a>b Then b=b+m
If a=b Then Return 0
If Abs(b-a)<=Abs(m+a-b) Then Return 1 Else Return -1

End Function



Function coord_to_angle#(x#,y#) ' given a couple, returns angle , like atan only better!

If x=0 
 If y=0 Then Return 0
 If y>0 Then Return 90
 If y<0 Then Return 270
EndIf
If y=0
 If x>0 Then Return 0
 If x<0 Then Return 180
EndIf

a#=ATan(y/x)
If x<0 Then a=a+180
If y<0 And x>0 Then a=a+360
Return a

End Function


'Print highest_binary(16)
Function highest_binary(n) ' returns highest binary in expo, e.g. 15=>3
' 0=>-1 , 1=>0 
While n>0
 n=n/2
 p=p+1
Wend

Return p-1

End Function



Function lcd(a,b) ' lowest common denomintor

If a=0 Or b=0 Then RuntimeError "lcd passed zero";Return 0
a=Abs(a)
b=Abs(b)
If (a Mod b)=0 Or (b Mod a)=0 Then Return fmax(a,b) Else Return a*b

End Function



Function high_prime(n) ' finds highest prime divisor of n ;desn't work
					   ' e.g. f(37)=1 f(100)=
s0=Floor(Float(Sqr(n)))
While (n Mod s0)<>0
 s0=s0-1
 If s0=1 Then Exit
Wend

Return s0

End Function



Function low_div(n) ' finds the lowest divisor of n

i=2

While (n Mod i)<>0
 i=i+1
Wend
Return i

End Function


Function high_div(n,l)  ' finds the highest divisor of n up to a limit of l.
						' e.g. f(68,5)=4 f(64,12)=8
While (n Mod l)<>0
 l=l-1
Wend
Return l

End Function


'x3#=49.0/36.0
'x5#=-14.0/36.0
'x7#=1.0/36.0
'Graphics 1024,768
'SetBuffer FrontBuffer()
'display_func(1,0,0,0,0,0,0,0) ;0,0,x3,0,x5,0,x7)
'display_func(0,0,0,0,0,0,0,1,0,0)

Function display_func(x1#,x2#,x3#,x4#,x5#,x6#,x7#,sx#,rx#,rx2#)
'r= reciprocol x
x#=-5

SetColor 222,222,222
DrawLine 512,0,512,768
DrawLine 0,384,1024,384
For x=-5 To 5
 DrawLine 512+x*102.4,384,512+x*102.4,390
Next
For y=-4 To 4
 DrawLine 505,y*100+384,512,y*100+384
Next

x=-5
While x<5
 
 'f#=rx2/(x*x)+rx/x+sx*Sin(x*180/Pi)+x7*x^7+x6*x^6+x5*x^5+x4*x^4+x3*x^3+x2*x^2+x1*x
 f#=(2*x+3)/(x^2-x+1)
 DebugLog x+"   "+f'rx2/(x*x)
 Plot 512+x*102.4,-100*f+384
 x=x+0.01
Wend

WaitMouse()
End

End Function



Function decide_graph(xsize,ysize,max_x,min_x,max_y,min_y)
' auto choose a graph to fit data

'draw_graph(xaxis#,yaxis#,xsize#,ysize#,xscale#,yscale#,font,bigfont,de,sc)

End Function


Function draw_graph(xaxis#,yaxis#,xsize#,ysize#,xscale#,yscale#,font,bigfont,de,sc)
' in the cntre of the screen
' side=1 or centre=0

If sc
 xsize=2*xsize
 ysize=2*ysize
EndIf

'SetFont font

SetColor 222,222,222
DrawLine xaxis,yaxis-ysize/2,xaxis,yaxis+ysize/2
DrawLine xaxis-xsize/2,yaxis,xaxis+xsize/2,yaxis

' len ; distance in pixels between consecutive graph lines
' no ; number of graph lines
' inc ; differnce in value between consecutive graph lines
' scale ; maximum number of graph

xinc=Ceil(Float(xscale)/4)
yinc=Ceil(Float(yscale)/4)

nox=Ceil(Float(xscale)/xinc)
noy=Ceil(Float(yscale)/yinc)

If nox<1 Then nox=1
If nox>10 Then nox=10
If noy<1 Then noy=1
If noy>10 Then noy=10

' this assumes large size for large number of axis points. Otherwise it's a bit cramped.

xlen#=xinc*(xsize/2)/xscale
ylen#=yinc*(ysize/2)/yscale

'DebugLog "nox="+nox+" xscale="+xscale+" xinc="+xinc+" xlen="+xlen

' xscale
x#=-(xsize/2)
i=-xscale
While x<=-0.1
 SetColor 70,70,70
 DrawLine x+xaxis,0,x+xaxis,GraphicsHeight()
 SetColor 222,222,222
 DrawLine x+xaxis,yaxis,x+xaxis,yaxis+5
 If de Then j=i*60 Else j=i
 If i<>0 Then DrawText x+xaxis,yaxis+5,j
 x=x+xlen
 i=i+xinc
Wend
x#=xsize/2
i=xscale
While x>=0.1
 SetColor 70,70,70
 DrawLine x+xaxis,0,x+xaxis,GraphicsHeight()
 SetColor 222,222,222
 DrawLine x+xaxis,yaxis,x+xaxis,yaxis+5
 If de Then j=i*60 Else j=i
 If i<>0 Then DrawText x+xaxis,yaxis+5,j
 x=x-xlen
 i=i-xinc
Wend

y#=0
i=yscale
While y<=ysize+0.1
 If y<(ysize/2)-5 Or y>(ysize/2)+5
  SetColor 70,70,70
  DrawLine 0,y+yaxis-ysize/2,GraphicsWidth(),y+yaxis-ysize/2
  SetColor 222,222,222
  DrawLine xaxis,y+yaxis-ysize/2,xaxis-5,y+yaxis-ysize/2
 EndIf
 DrawText xaxis-20,3+y+yaxis-ysize/2,i
 i=i-yinc
 y=y+ylen
Wend

'SetFont bigfont
'arrows
DrawLine xsize/2+xaxis,yaxis,xsize/2+xaxis+20,yaxis
DrawLine xsize/2+xaxis+20,yaxis,xsize/2+xaxis+10,yaxis-10
DrawLine xsize/2+xaxis+20,yaxis,xsize/2+xaxis+10,yaxis+10
DrawText "X",xsize/2+xaxis+30,yaxis

SetColor 222,222,222
DrawLine xaxis,yaxis-(20+ysize/2),xaxis,yaxis-ysize/2
DrawLine xaxis,yaxis-(20+ysize/2),xaxis-10,-10+yaxis-ysize/2
DrawLine xaxis,yaxis-(20+ysize/2),xaxis+10,-10+yaxis-ysize/2
DrawText "Y",xaxis,yaxis-(40+ysize/2)

End Function




Function peekhyte(bank,pos)

b=PeekByte(bank,Floor(pos/2))
p=pos Mod 2
If p=0 Then Return b Mod 16
If p=1 Then Return b Shr 4

End Function



Function pokehyte(bank,pos,x)

b=PeekByte(bank,Floor(pos/2))
p=pos Mod 2
If p=0 Then b=(b & %11110000) | x
If p=1 Then b=(b & %00001111) | (x Shl 4)
PokeByte(bank,Floor(pos/2),b)

End Function



Function peekbit(bank,pos)

b=PeekByte(bank,Floor(pos/8))
c=1 Shl (pos Mod 8)
Return (b & c)

End Function



Function pokebit(bank,pos,x)

If x=0 Then pokebitoff(bank,pos);Return
y=pos Mod 8
z=PeekByte(bank,Floor(pos/8))
c=1 Shl y
d=z | c
PokeByte(bank,Floor(pos/8),d)

End Function



Function pokebitoff(bank,pos)

b=1 Shl (pos Mod 8)
z=PeekByte(bank,Floor(pos/8))
p=255 ~ b
z=z & p
PokeByte(bank,Floor(pos/8),z)

End Function



Type bbcolumn Extends TBBType

	Method New()
		Add(column_list)
	End Method

	Method After:bbcolumn()
		Local t:TLink
		t=_link.NextLink()
		If t Return bbcolumn(t.Value())
	End Method

	Method Before:bbcolumn()
		Local t:TLink
		t=_link.PrevLink()
		If t Return bbcolumn(t.Value())
	End Method

Field row#[2]
End Type

Type bbmatrix Extends TBBType

	Method New()
		Add(matrix_list)
	End Method

	Method After:bbmatrix()
		Local t:TLink
		t=_link.NextLink()
		If t Return bbmatrix(t.Value())
	End Method

	Method Before:bbmatrix()
		Local t:TLink
		t=_link.PrevLink()
		If t Return bbmatrix(t.Value())
	End Method

Field col:bbcolumn[2]
End Type



Function create_matrix:bbmatrix()

Local m:bbmatrix=New bbmatrix

For a=0 To 2
 c:bbcolumn=New bbcolumn
 m.col[a]=c 
Next

Return m

End Function



Function create_unit_matrix:bbmatrix()

Local u:bbmatrix=create_matrix()

u.col[0].row[0]=1
u.col[1].row[1]=1
u.col[2].row[2]=1

Return u

End Function



Function create_user_matrix:bbmatrix()

Local m:bbmatrix=create_matrix()

m.col[0].row[0]=0
m.col[0].row[1]=0
m.col[0].row[2]=1

m.col[1].row[0]=1
m.col[1].row[1]=0
m.col[1].row[2]=0

m.col[2].row[0]=0
m.col[2].row[1]=1
m.col[2].row[2]=0

Return m

End Function



'm1.matrix=create_unit_matrix()
'm2.matrix=create_user_matrix()
'display_matrix(m2)
'matrix_mult(m2,m2)
'display_matrix(m2)

Function display_matrix(m:bbmatrix)

For a=0 To 2
 Print m.col[0].row[a]+"  "+m.col[1].row[a]+"  "+m.col[2].row[a]
Next

End Function



Function matrix_set_equal(Too:bbmatrix,from:bbmatrix)

For c=0 To 2
For r=0 To 2
 j#=from.col[c].row[r]
 too.col[c].row[r]=j
Next
Next

End Function



Function matrix_mult(top:bbmatrix,under:bbmatrix)

top_old:bbmatrix=create_matrix()
matrix_set_equal(top_old,top)

under_old:bbmatrix=create_matrix()
matrix_set_equal(under_old,under)

For cu=0 To 2
For ct=0 To 2
a#=0
For r=0 To 2
a#=a+top_old.col[r].row[ct]*under_old.col[cu].row[r]
Next
under.col[cu].row[ct]=a
Next
Next

Delete_matrix(top_old)
Delete_matrix(under_old)

End Function



Function col_transform(m:bbmatrix,col:bbcolumn)

old_col:bbcolumn=New bbcolumn
old_col.row[0]=col.row[0]
old_col.row[1]=col.row[1]
old_col.row[2]=col.row[2]

For c=0 To 2
a#=0
For r=0 To 2
a#=a+m.col[r].row[c]*old_col.row[r]
Next
col.row[c]=a
Next

old_col.Remove()

End Function



Function delete_matrix(m:bbmatrix)

For c=0 To 2;m.col[c].Remove();Next
m.Remove()

End Function



Function highest_bit(x)
' discrete version the inverse of to the power of 2
' uses bit bank
While x<>0
 x=x Shr 1
 i=i+1
Wend
Return i

End Function



Global rnd_seed
Function dep_Rand(low,high)

rnd_seed=Rnd_seed+1

ss#=Sqr(rnd_seed)+(Float(rnd_seed))^0.37
ss2#=ss*10000
a=ss2

b=a'Abs(a*456377) ;And 32767
range=(high-low)+1
b=(b Mod range)+low
Return b

End Function



Function dep_rnd#(low#,high#)

rnd_seed=rnd_seed+1

'Stop
ss#=Sqr(rnd_seed)+Sqr(rnd_seed+Pi)
ss=ss-Int(ss)
ss=Abs(ss*100000)
ss=Sqr(ss)+Sqr(ss+Pi)
ss=ss-Floor(ss)
ss=ss*(high-low)-low

Return ss

End Function



Function get_nth_bit(n)

b=n/31
c=n Mod 31
d=Sgn(bit_bank[b] & (1 Shl c))
Return d

End Function



Function set_nth_bit(n,x)

b=n/31
c=n Mod 31
If bit_bank[b] & (1 Shl c) Then bit_bank[b]=bit_bank[b]-(1 Shl c)
bit_bank[b]=bit_bank[b]+(x Shl c)

End Function



Function display_bit_bank_matrix(x2,y2)

For y=0 To y2-1
For x=0 To x2-2
 z=y*x2+x
' Stop
 Print get_nth_bit(z)+" "
Next
z=y*x2+x
Print get_nth_bit(z)+" "
Next

End Function
