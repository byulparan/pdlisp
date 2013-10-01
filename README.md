<p>PDLISP - CommonLisp in PureData.</p>
<hr />
<p>PDLISP is PureData object for Lisp programmer.<br />
It include codes from pdlua.<br />
also, It include ecl header files. because name conflict.(pd and ecl).<br />
I can't avoid this problem. so I modified ecl's header file.<br />
</p>

<p>currently It support OSX / Linux.<br />
I don't have Window programming skill.
</p>

<p>Binary(only i386)<br />
&nbsp; &nbsp;&nbsp; &nbsp;OSX &nbsp; :  https://dl.dropboxusercontent.com/u/23310809/pdlisp-darwin.tar <br />
&nbsp; &nbsp;&nbsp; &nbsp;Linux : https://dl.dropboxusercontent.com/u/23310809/pdlisp-linux.tar<br />
</p>

<p> If you want build from sources...</p>
<p> Requires : <br />
&nbsp; &nbsp;PD or PD-extended and PD sources - http://puredata.info <br />
&nbsp; &nbsp;ECL(Embeddable Common Lisp) - http://ecls.sourceforge.net<br />
&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;..(I build ECL for static version. use without-shared options)
</p>
<p>  install :<br />
&nbsp; &nbsp;You must set environment variables PD_INCLUDE and ECL_LIBS.<br />
&nbsp; &nbsp;It's path to pd(m_pd.h) and ecl.<br />
&nbsp; &nbsp;example) export PD_INCLUDE=/usr/include/pd<br />
&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;export ECL_LIBS = /usr/local/lib/ecl-13.5.1<br />
&nbsp; &nbsp;then. just type make.<br />
