<%@ Page Language="VB" %>

<!DOCTYPE html>

<script runat="server">

    Public Function delete_customer_file() As String

        Dim customer_id_num_h As String = mov_id_text.Text

        If System.IO.File.Exists("/custom_data/" & customer_id_num_h & ".txt") = True Then

            System.IO.File.Delete("/custom_data/" & customer_id_num_h & ".txt")

        End If


        Return customer_id_num_h

    End Function

    Public Function write_ticket_price_mix(ByVal ticket_mix As Integer) As String

        Dim customer_id_num_h As String = mov_id_text.Text
        Dim myString As String = ""
        Dim chMovieName As String = ""
        Dim enMovieName As String = ""
        Dim theater As String = ""
        Dim day As String = ""
        Dim time As String = ""
        Dim price_mix As String = ticker_price_mix_5.Text

        FileOpen(1, Server.MapPath("/custom_data/" & customer_id_num_h & ".txt"), OpenMode.Input)

        chMovieName = LineInput(1)
        enMovieName = LineInput(1)
        theater = LineInput(1)
        day = LineInput(1)
        time = LineInput(1)

        FileClose(1)


        FileOpen(2, Server.MapPath("/custom_data/" & customer_id_num_h & ".txt"), OpenMode.Output)

        myString = myString & chMovieName & Chr(13)
        myString = myString & enMovieName & Chr(13)
        myString = myString & theater & Chr(13)
        myString = myString & day & Chr(13)
        myString = myString & time & Chr(13)
        myString = myString & customer_id_num_h & Chr(13)
        myString = myString & price_mix & Chr(13)
        myString = myString & ticket_mix & Chr(13)


        Print(2, myString)
        FileClose(2)

        Return myString

    End Function

    Protected Sub Page_Load(sender As Object, e As EventArgs)

        Dim customer_id_num_h As String = Application("customer_id_num")
        time_int.Text = Application("scene_num")
        day_int.Text = Application("day_num")
        Dim chMovieName As String = ""
        Dim enMovieName As String = ""
        Dim theater As String = ""
        Dim day As String = ""
        Dim time As String = ""
        Dim ticker_price_A As Integer = ticker_price_1.Text
        Dim ticker_price_B As Integer = ticker_price_2.Text
        Dim ticker_price_C As Integer = ticker_price_3.Text
        Dim ticker_price_D As Integer = ticker_price_4.Text
        Dim ticker_price_mix_A As Integer = 0
        Dim ticker_price_mix_B As Integer = 0
        Dim ticker_price_mix_C As Integer = 0
        Dim ticker_price_mix_D As Integer = 0
        Dim ticker_price_All As Integer = 0
        Dim ticker_A As Integer = 0
        Dim ticker_B As Integer = 0
        Dim ticker_C As Integer = 0
        Dim ticker_D As Integer = 0
        Dim ticker_E As Integer = 0

        FileOpen(1, Server.MapPath("/custom_data/" & customer_id_num_h & ".txt"), OpenMode.Input)

        chMovieName = LineInput(1)
        enMovieName = LineInput(1)
        theater = LineInput(1)
        day = LineInput(1)
        time = LineInput(1)

        FileClose(1)

        mov_ch_text.Text = chMovieName
        mov_en_text.Text = enMovieName
        theater_text.Text = theater
        day_text.Text = day
        time_text.Text = time
        mov_id_text.Text = customer_id_num_h

        ticker_price_mix_1.Text = ticket_sel1.SelectedIndex * ticker_price_A
        ticker_price_mix_A = ticker_price_mix_1.Text
        ticker_price_mix_2.Text = ticket_sel2.SelectedIndex * ticker_price_B
        ticker_price_mix_B = ticker_price_mix_2.Text
        ticker_price_mix_3.Text = ticket_sel3.SelectedIndex * ticker_price_C
        ticker_price_mix_C = ticker_price_mix_3.Text
        ticker_price_mix_4.Text = ticket_sel4.SelectedIndex * ticker_price_D
        ticker_price_mix_D = ticker_price_mix_4.Text
        ticker_price_mix_5.Text = ticker_price_mix_A + ticker_price_mix_B + ticker_price_mix_C + ticker_price_mix_D
        ticker_price_All = ticker_price_mix_5.Text
        ticker_A = ticket_sel1.SelectedIndex
        ticker_B = ticket_sel2.SelectedIndex
        ticker_C = ticket_sel3.SelectedIndex
        ticker_D = ticket_sel4.SelectedIndex
        ticker_E = ticker_A + ticker_B + ticker_C + ticker_D

    End Sub

    Protected Sub Index_logo_Click(sender As Object, e As ImageClickEventArgs)

        delete_customer_file()
        Response.Redirect("movindex.aspx")

    End Sub

    Protected Sub ticker_price_back_Click(sender As Object, e As EventArgs)

        delete_customer_file()
        Response.Redirect("movindex.aspx")


    End Sub

    Protected Sub ticker_price_next_Click(sender As Object, e As EventArgs)

        Application("frist_load") = 0
        Dim customer_id_num_h As String = mov_id_text.Text
        Dim ticker_A As Integer = ticket_sel1.SelectedIndex
        Dim ticker_B As Integer = ticket_sel2.SelectedIndex
        Dim ticker_C As Integer = ticket_sel3.SelectedIndex
        Dim ticker_D As Integer = ticket_sel4.SelectedIndex
        Dim ticket_mix As Integer = ticker_A + ticker_B + ticker_C + ticker_D

        Application("ticket") = ticket_mix

        If ticket_mix >= 1 Then

            Application("customer_id_num_seat") = customer_id_num_h
            Application("day_num_step2") = day_int.Text
            Application("scene_num_step2") = time_int.Text
            write_ticket_price_mix(ticket_mix)
            Response.Redirect("new_movseat.aspx")

        ElseIf ticket_mix = 0 Then

            massage_01.ForeColor = Drawing.Color.Red
            massage_01.Text = "請選擇購票張數"

        End If


    End Sub

</script>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
  <title>映影城-InMovie-購票區</title>
    <style type="text/css">
        .auto-style1 {
            height: 1426px;
        }
        .auto-style6 {
            height: 166px;
        }
        .auto-style7 {
            height: 165px;
        }
      .auto-style23 {
          width: 1277px;
          height: 24px;
      }
      * {
    color: #000 !important;
    text-shadow: none !important;
    background: transparent !important;
    -webkit-box-shadow: none !important;
            box-shadow: none !important;
  }
      * {
  -webkit-box-sizing: border-box;
     -moz-box-sizing: border-box;
          box-sizing: border-box;
}
        .auto-style24 {
            height: 197px;
            margin-top: 10px;
        }
        .auto-style25 {
            width: 42px;
        }
        .auto-style26 {
            width: 591px;
            height: 166px;
            margin-left: 327px;
        }
        .auto-style27 {
            height: 50px;
        }
        .auto-style28 {
            height: 808px;
        }
        .auto-style29 {
            margin-left: 288px;
            margin-top: 92px;
        }
        .auto-style30 {
            width: 100%;
            height: 0px;
            margin-top: 24px;
        }
        .auto-style31 {
            width: 100%;
            height: 506px;
        }
        .auto-style36 {
            width: 136px;
        }
        .auto-style38 {
            width: 258px;
        }
        .auto-style39 {
            width: 147px;
        }
        .auto-style40 {
            width: 860px;
            height: 88px;
        }
        .auto-style41 {
            width: 136px;
            margin-left: 509px;
        }
        .auto-style42 {
            height: 184px;
        }
        .auto-style43 {
            width: 415px;
        }
        .auto-style44 {
            width: 306px;
        }
        .auto-style45 {
            width: 89px;
        }
    </style>
</head>
<body style="height: 1428px">
  <form id="form1" runat="server">
  <div id="header" class="auto-style1">
  
  <div id="Header0" style="margin-top: 0px; background-color: #333333;" class="auto-style6">
      <div style="margin-left: 192px; margin-right: 0px; margin-top: 0px;" class="auto-style7">
          <div style="height: 163px; width: 997px; margin-left: 0px; margin-right: 0px; margin-top: 0px">
              <asp:ImageButton ID="Index_logo" runat="server" Height="145px" ImageUrl="~/images/logo.png" style="margin-top: 10px" Width="181px" OnClick="Index_logo_Click" />
          </div>
      </div>
      <div>
      <div class="auto-style23">
          <asp:SiteMapPath ID="SiteMapPath1" runat="server">
          </asp:SiteMapPath>
          <br />
          <div class="auto-style24">
      <div class="auto-style5">
                  <table class="auto-style26">
                      <tr>
                          <td class="auto-style44" >
                              <asp:Label ID="mov_ch_text" runat="server" Text="空白"></asp:Label>
                              <br />
                          </td>
                          <td class="auto-style45" colspan="1" "></td>
                          <td class="auto-style9">
                              <asp:Label ID="day_text" runat="server" Text="time"></asp:Label>
                              <asp:Label ID="day_int" runat="server" Text="time" Visible="False"></asp:Label>
                              <br />
                          </td>
                          <td class="auto-style25" "></td>
                          <td class="auto-style9">
                              &nbsp;</td>
                      </tr>
                      <tr>
                          <td class="auto-style44" >
                              <asp:Label ID="mov_en_text" runat="server" Text="空白"></asp:Label>
                          </td>
                          <td class="auto-style45" colspan="1" "></td>
                          <td class="auto-style9">
                              <asp:Label ID="time_text" runat="server" Text="time"></asp:Label>
                              <asp:Label ID="time_int" runat="server" Text="time" Visible="False"></asp:Label>
                          </td>
                          <td class="auto-style25" "></td>
                          <td class="auto-style9">
                              &nbsp;</td>
                      </tr>
                      <tr>
                          <td class="auto-style44" >
                              訂票編號:
                              <asp:Label ID="mov_id_text" runat="server" Text="空白"></asp:Label>
                          </td>
                          <td class="auto-style45" colspan="1" >
                              &nbsp;</td>
                          <td class="auto-style25" >
                              <asp:Label ID="theater_text" runat="server" Text="theater"></asp:Label>
                          </td>
                      </tr>
                      </table>
                  <hr class="auto-style30" />
              </div>
          </div>
          <div class="auto-style27">
        <asp:ScriptManager ID="ScriptManager1" runat="server">
        </asp:ScriptManager>
          </div>
          <div class="auto-style28">
              <asp:Panel ID="Panel1" runat="server" CssClass="auto-style29" Height="540px" Width="696px">
                  <table class="auto-style31">
                      <tr>
                          <td class="auto-style38">票種</td>
                          <td class="auto-style39">價格</td>
                          <td class="auto-style36">張數</td>
                          <td>小計</td>
                      </tr>
                      <tr>
                          <td class="auto-style38">全票</td>
                          <td class="auto-style39">
                              <div>
                                  <asp:Label ID="ticker_price_1" runat="server" Text="270"></asp:Label>
                                  元</div>
                          </td>
                          <td class="auto-style36">
                              <asp:UpdatePanel ID="UpdatePanel6" runat="server">
                                  <ContentTemplate>
                                      <asp:DropDownList ID="ticket_sel1" runat="server" AutoPostBack="True" Height="18px" Width="75px">
                                          <asp:ListItem Value="0"></asp:ListItem>
                                          <asp:ListItem Value="1"></asp:ListItem>
                                          <asp:ListItem Value="2"></asp:ListItem>
                                          <asp:ListItem Value="3"></asp:ListItem>
                                          <asp:ListItem Value="4"></asp:ListItem>
                                          <asp:ListItem Value="5"></asp:ListItem>
                                      </asp:DropDownList>
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                          <td>
                              <asp:UpdatePanel ID="UpdatePanel1" runat="server">
                                  <ContentTemplate>
                                      <asp:Label ID="ticker_price_mix_1" runat="server" Text="0"></asp:Label>
                                      &nbsp; 元
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                      </tr>
                      <tr>
                          <td class="auto-style38">學生票</td>
                          <td class="auto-style39">
                              <div>
                                  <asp:Label ID="ticker_price_2" runat="server" Text="220"></asp:Label>
                                  元</div>
                          </td>
                          <td class="auto-style36">
                              <asp:UpdatePanel ID="UpdatePanel7" runat="server">
                                  <ContentTemplate>
                                      <asp:DropDownList ID="ticket_sel2" runat="server" AutoPostBack="True" Height="18px" Width="75px">
                                          <asp:ListItem Value="0"></asp:ListItem>
                                          <asp:ListItem Value="1"></asp:ListItem>
                                          <asp:ListItem Value="2"></asp:ListItem>
                                          <asp:ListItem Value="3"></asp:ListItem>
                                          <asp:ListItem Value="4"></asp:ListItem>
                                          <asp:ListItem Value="5"></asp:ListItem>
                                      </asp:DropDownList>
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                          <td>
                              <asp:UpdatePanel ID="UpdatePanel2" runat="server">
                                  <ContentTemplate>
                                      <asp:Label ID="ticker_price_mix_2" runat="server" Text="0"></asp:Label>
                                      &nbsp; 元
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                      </tr>
                      <tr>
                          <td class="auto-style38">兒童票</td>
                          <td class="auto-style39">
                              <div>
                                  <asp:Label ID="ticker_price_3" runat="server" Text="150"></asp:Label>
                                  元</div>
                          </td>
                          <td class="auto-style36">
                              <asp:UpdatePanel ID="UpdatePanel8" runat="server">
                                  <ContentTemplate>
                                      <asp:DropDownList ID="ticket_sel3" runat="server" AutoPostBack="True" Height="18px" Width="75px">
                                          <asp:ListItem Value="0"></asp:ListItem>
                                          <asp:ListItem Value="1"></asp:ListItem>
                                          <asp:ListItem Value="2"></asp:ListItem>
                                          <asp:ListItem Value="3"></asp:ListItem>
                                          <asp:ListItem Value="4"></asp:ListItem>
                                          <asp:ListItem Value="5"></asp:ListItem>
                                      </asp:DropDownList>
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                          <td>
                              <asp:UpdatePanel ID="UpdatePanel3" runat="server">
                                  <ContentTemplate>
                                      <asp:Label ID="ticker_price_mix_3" runat="server" Text="0"></asp:Label>
                                      &nbsp; 元
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                      </tr>
                      <tr>
                          <td class="auto-style38">優待票</td>
                          <td class="auto-style39">
                              <div>
                                  <asp:Label ID="ticker_price_4" runat="server" Text="200"></asp:Label>
                                  元</div>
                          </td>
                          <td class="auto-style36">
                              <asp:UpdatePanel ID="UpdatePanel9" runat="server">
                                  <ContentTemplate>
                                      <asp:DropDownList ID="ticket_sel4" runat="server" AutoPostBack="True" Height="18px" Width="75px">
                                          <asp:ListItem Value="0"></asp:ListItem>
                                          <asp:ListItem Value="1"></asp:ListItem>
                                          <asp:ListItem Value="2"></asp:ListItem>
                                          <asp:ListItem Value="3"></asp:ListItem>
                                          <asp:ListItem Value="4"></asp:ListItem>
                                          <asp:ListItem Value="5"></asp:ListItem>
                                      </asp:DropDownList>
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                          <td>
                              <asp:UpdatePanel ID="UpdatePanel4" runat="server">
                                  <ContentTemplate>
                                      <asp:Label ID="ticker_price_mix_4" runat="server" Text="0"></asp:Label>
                                      &nbsp; 元
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                      </tr>
                      <tr>
                          <td class="auto-style38">&nbsp;</td>
                          <td class="auto-style39">總價</td>
                          <td class="auto-style36">&nbsp;</td>
                          <td>
                              <asp:UpdatePanel ID="UpdatePanel5" runat="server">
                                  <ContentTemplate>
                                      <asp:Label ID="ticker_price_mix_5" runat="server" Text="0"></asp:Label>
                                      &nbsp;&nbsp; 元
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                          </td>
                      </tr>
                  </table>
                  <br />
                  <asp:Label ID="massage_01" runat="server" ForeColor="Red" Text="請選擇張數"></asp:Label>
                  <br />
                  <br />
                  <div class="auto-style40">
                      <asp:Button ID="ticker_price_back" runat="server" CssClass="auto-style41" Height="38px" Text="回上一步" Width="159px" OnClick="ticker_price_back_Click" />
                      <asp:Button ID="ticker_price_next" runat="server" CssClass="auto-style43" Height="38px" style="margin-left: 41px" Text="下一步" Width="138px" OnClick="ticker_price_next_Click" />
                  </div>
              </asp:Panel>
          </div>
          <div class="auto-style42">
          </div>
          <br />
      </div>
       </div>
      </div>
  
  </div>
  </form>
</body>
</html>
