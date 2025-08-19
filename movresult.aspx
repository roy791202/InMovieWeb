<%@ Page Language="VB" %>

<%@ Register assembly="System.Web.DataVisualization, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35" namespace="System.Web.UI.DataVisualization.Charting" tagprefix="asp" %>

<!DOCTYPE html>

<script runat="server">

    Public Function result_page_load() As Integer

        Dim num As Integer = 0
        Dim customer_id_num_h As String = Application("result_id")
        Dim customer_id As String = ""
        Dim chMovieName As String = ""
        Dim enMovieName As String = ""
        Dim theater As String = ""
        Dim day As String = ""
        Dim time As String = ""
        Dim ticket_price_mix As String = ""
        Dim ticket_mix As String = ""
        Dim seat_id As String = ""

        FileOpen(1, Server.MapPath("/custom_data/" & customer_id_num_h & ".txt"), OpenMode.Input)

        chMovieName = LineInput(1)
        enMovieName = LineInput(1)
        theater = LineInput(1)
        day = LineInput(1)
        time = LineInput(1)
        customer_id = LineInput(1)
        ticket_price_mix = LineInput(1)
        ticket_mix = LineInput(1)

        Do While Not EOF(1)

            seat_id = seat_id & LineInput(1) & Chr(13) & Chr(13)

        Loop

        FileClose(1)

        mov_id.Text = customer_id_num_h
        ch_name.Text = chMovieName
        en_name.Text = enMovieName
        mov_s.Text = theater
        mov_time2.Text = day & Chr(13) & time
        seat_inf.Text = seat_id

        Application("result_load") = 1
        num = Application("frist_load")

        Return num

    End Function

    Protected Sub Page_Load(sender As Object, e As EventArgs)

        If Application("result_load") = 0 Then

            result_page_load()

        End If

    End Sub

    Protected Sub result_check_Click(sender As Object, e As EventArgs)

        Response.Redirect("movindex.aspx")

    End Sub
</script>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
  <title>映影城-InMovie-訂票結果區</title>
  <style type="text/css">
<!--
#index_form #Header {
	background-color: #333333;
}
#index_form #Footer {
	background-color: #666666;
}
    #Button5 {
	margin-top: 20px;
	word-spacing: inherit;
	width: auto;
          margin-left: 607px;
          margin-bottom: 0px;
      }
#index_form #Body1 {
	background-image: url(images/uybmnef1nha4xqbvbqty.jpg);
}
      .auto-style17 {
          height: 572px;
          width: 1013px;
          margin-left: 167px;
          margin-top: 90px;
      }
      .auto-style19 {
          height: 175px;
      }
      .auto-style21 {
          height: 166px;
          width: 995px;
      }
      .auto-style22 {
          height: 945px;
          margin-top: 60px;
      }
      .auto-style23 {
          height: 66px;
          margin-top: 3px;
      }
      .auto-style25 {
          height: 52px;
          margin-top: 10px;
      }
      .auto-style26 {
          width: 56%;
          height: 527px;
          margin-left: 264px;
      }
      .auto-style29 {
          width: 130px;
          height: 144px;
      }
      .auto-style32 {
          width: 130px;
          height: 112px;
      }
      .auto-style34 {
          height: 112px;
      }
      .auto-style35 {
          height: 144px;
      }
      .auto-style36 {
          width: 130px;
          height: 136px;
      }
      .auto-style38 {
          height: 136px;
      }
      .auto-style39 {
          width: 68px;
          height: 144px;
      }
      .auto-style40 {
          width: 68px;
          height: 112px;
      }
      .auto-style41 {
          width: 68px;
          height: 136px;
      }
      .auto-style42 {
          margin-left: 847px;
          margin-top: 11px;
      }
        .auto-style200 {
            height: 177px;
          width: 1181px;
      }
        .auto-style7 {
          margin-left: 289px;
      }
      -->
  </style>
</head>
<body style="height: 1549px; margin-top: 0px;">
  <form id="index_form" runat="server">
  <div id="Header" style="margin-top: 0px" class="auto-style19">
      <div style="margin-left: 192px; margin-right: 0px; margin-top: 0px;" class="auto-style21">
          <div style="height: 163px; width: 997px; margin-left: 0px; margin-right: 0px; margin-top: 0px">
              <asp:ImageButton ID="ImageButton1" runat="server" Height="145px" ImageUrl="~/images/logo.png" style="margin-top: 10px" Width="181px" />
          <asp:Button ID="Button3" runat="server" Height="27px" Text="影城介紹" Width="150px" BackColor="#333333" BorderColor="#333333" BorderStyle="Solid" ForeColor="#FFCC00" style="margin-left: 93px" />
          <asp:Button ID="Button2" runat="server" Height="27px" Text="電影介紹" Width="150px" BackColor="#333333" BorderColor="#333333" BorderStyle="Solid" ForeColor="#FFCC00" />
          <asp:Button ID="Button1" runat="server" Height="27px" Text="熱門討論區" Width="150px" BackColor="#333333" BorderColor="#333333" BorderStyle="Solid" ForeColor="#FFCC00" />
          <asp:Button ID="Button4" runat="server" Height="27px" Text="業務專區" Width="150px" BackColor="#333333" BorderColor="#333333" BorderStyle="Solid" ForeColor="#FFCC00" />
          </div>
      </div>
      <div class="auto-style23">
      <div class="auto-style25">
          <asp:SiteMapPath ID="SiteMapPath1" runat="server">
          </asp:SiteMapPath>
      </div>
      </div>
      </div>
      <div id="Body2" class="auto-style22">
          <br />
          <div class="auto-style17">
              <table class="auto-style26">
                  <tr>
                      <td class="auto-style29" style="border-color: #000000; background-color: #99CCFF">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 訂票碼:</td>
                      <td class="auto-style39"></td>
                      <td class="auto-style35">
                          <asp:Label ID="mov_id" runat="server" Font-Size="Large" Text="空白"></asp:Label>
                      </td>
                  </tr>
                  <tr>
                      <td class="auto-style29" style="background-color: #99CCFF">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 片名:</td>
                      <td class="auto-style39"></td>
                      <td class="auto-style35">
                              <asp:Label ID="ch_name" runat="server" Text="空白" Font-Size="Large"></asp:Label>
                          <br />
                              <asp:Label ID="en_name" runat="server" Text="空白" Font-Size="Large"></asp:Label>
                          <br />
                      </td>
                  </tr>
                  <tr>
                      <td class="auto-style32" style="background-color: #99CCFF">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 影片資訊:</td>
                      <td class="auto-style40"></td>
                      <td class="auto-style34">
                          <br />
                          <asp:Label ID="mov_s" runat="server" Font-Size="Large" Text="空白"></asp:Label>
                      </td>
                  </tr>
                  <tr>
                      <td class="auto-style36" style="background-color: #99CCFF">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 場次日期:</td>
                      <td class="auto-style41"></td>
                      <td class="auto-style38">
                          <asp:Label ID="mov_time2" runat="server" Font-Size="Large" Text="空白"></asp:Label>
                          <br />
                          <br />
                          <asp:Label ID="seat_inf" runat="server" Font-Size="Large" Text="空白"></asp:Label>
                      </td>
                  </tr>
              </table>
              <br />
              <br />
              <div>
                  <asp:Button ID="result_check" runat="server" CssClass="auto-style42" Height="43px" Text="確認完畢" Width="160px" OnClick="result_check_Click" />
              </div>
          </div>
      </div>
    <div class="auto-style200" style="background-color: #333333">
        <br />
        <br />
        <br />
          <asp:Image ID="wed_id" runat="server" CssClass="auto-style7" Height="77px" ImageUrl="~/images/web_id.jpg" Width="80px" />
          &nbsp;&nbsp;
          <asp:Label ID="Label1" runat="server" ForeColor="White" Text="Copyright © 2017 映電影娛樂 INMOVIES ENTERTAINMENT. All Rights Reserved."></asp:Label>
          <br />
    </div>
  </form>
</body>
</html>
