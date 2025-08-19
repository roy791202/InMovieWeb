<%@ Page Language="VB" %>

<!DOCTYPE html>

<script runat="server">

    Public Function customer_id(ByVal num As Integer) As String

        Dim myString = ""

        If num = 1 Then
            myString = "A"
        ElseIf num = 2 Then
            myString = "B"
        ElseIf num = 3 Then
            myString = "C"
        ElseIf num = 4 Then
            myString = "D"
        ElseIf num = 5 Then
            myString = "E"
        ElseIf num = 6 Then
            myString = "F"
        ElseIf num = 7 Then
            myString = "G"
        ElseIf num = 8 Then
            myString = "H"
        ElseIf num = 9 Then
            myString = "I"
        ElseIf num = 10 Then
            myString = "J"
        ElseIf num = 11 Then
            myString = "K"
        ElseIf num = 12 Then
            myString = "L"
        ElseIf num = 13 Then
            myString = "M"
        ElseIf num = 14 Then
            myString = "N"
        ElseIf num = 15 Then
            myString = "O"
        ElseIf num = 16 Then
            myString = "P"
        ElseIf num = 17 Then
            myString = "Q"
        ElseIf num = 18 Then
            myString = "R"
        ElseIf num = 19 Then
            myString = "S"
        ElseIf num = 20 Then
            myString = "T"
        ElseIf num = 21 Then
            myString = "U"
        ElseIf num = 22 Then
            myString = "V"
        ElseIf num = 23 Then
            myString = "W"
        ElseIf num = 24 Then
            myString = "X"
        ElseIf num = 25 Then
            myString = "Y"
        ElseIf num = 26 Then
            myString = "Z"
        End If

        Return myString

    End Function

    Public Function random_number() As String

        Dim myString = ""
        Randomize()
        Dim value1 As Integer = CInt(Int((9 * Rnd()) + 1))
        Randomize()
        Dim value2 As Integer = CInt(Int((9 * Rnd()) + 1))
        Randomize()
        Dim value3 As Integer = CInt(Int((9 * Rnd()) + 1))
        Randomize()
        Dim value4 As Integer = CInt(Int((9 * Rnd()) + 1))
        Randomize()
        Dim value5 As Integer = CInt(Int((26 * Rnd()) + 1))
        Randomize()
        Dim value6 As Integer = CInt(Int((9 * Rnd()) + 1))
        Randomize()
        Dim value7 As Integer = CInt(Int((9 * Rnd()) + 1))
        Randomize()
        Dim value8 As Integer = CInt(Int((26 * Rnd()) + 1))

        Dim text_id1 As String = customer_id(value5)
        Dim text_id2 As String = customer_id(value8)


        myString = value1 & value2 & value3 & value4 & text_id1 & text_id2 & value6 & value7

        Return myString

    End Function
    Public Function write_customer_number(ByVal movie As Integer, ByVal booking_day As String, ByVal booking_scene As String) As String

        Dim myString = ""
        Dim gsbnum As Integer
        Dim chMovieName(1000), enMovieName(1000), theaterNo(1000), showDay1(1000), showDay2(1000), showDay3(1000), showDay4(1000), showDay5(1000)
        Dim showTime1(1000), showTime2(1000), showTime3(1000), showTime4(1000), showTime5(1000), showTime6(1000), showTime7(1000), showTime8(1000)
        Dim dayN As Integer = day.SelectedIndex
        Dim sceneN As Integer = scene.SelectedIndex
        Dim booking_movie As String = ""
        Dim booking_theater As String = ""

        Dim movie_id As String = customer_id(movie)
        Dim day_id As String = customer_id(dayN)
        Dim scene_id As String = customer_id(sceneN)
        Dim num_id As String = random_number()
        Dim customer_id_num As String = movie_id & day_id & scene_id & num_id

        FileOpen(1, Server.MapPath("/movie_data/movie_name.txt"), OpenMode.Input)
        gsbnum = CInt(LineInput(1))

        For i = 1 To gsbnum

            chMovieName(i) = LineInput(1)
            enMovieName(i) = LineInput(1)
            theaterNo(i) = LineInput(1)
            showDay1(i) = LineInput(1)
            showDay2(i) = LineInput(1)
            showDay3(i) = LineInput(1)
            showDay4(i) = LineInput(1)
            showDay5(i) = LineInput(1)
            showTime1(i) = LineInput(1)
            showTime2(i) = LineInput(1)
            showTime3(i) = LineInput(1)
            showTime4(i) = LineInput(1)
            showTime5(i) = LineInput(1)
            showTime6(i) = LineInput(1)
            showTime7(i) = LineInput(1)
            showTime8(i) = LineInput(1)

        Next

        FileClose(1)

        booking_movie = chMovieName(movie + 1) & Chr(13) & enMovieName(movie + 1)
        booking_theater = theaterNo(movie + 1)


        FileOpen(2, Server.MapPath("/custom_data/" & customer_id_num & ".txt"), OpenMode.Output)

        myString = myString & booking_movie & Chr(13)
        myString = myString & booking_theater & Chr(13)
        myString = myString & booking_day & Chr(13)
        myString = myString & booking_scene & Chr(13)

        Print(2, myString)
        FileClose(2)

        Return customer_id_num

    End Function


    Public Function movie_load() As Integer

        Dim myString = ""
        Dim gsbnum As Integer
        Dim chMovieName(1000), enMovieName(1000), theaterNo(1000), showDay1(1000), showDay2(1000), showDay3(1000), showDay4(1000), showDay5(1000)
        Dim showTime1(1000), showTime2(1000), showTime3(1000), showTime4(1000), showTime5(1000), showTime6(1000), showTime7(1000), showTime8(1000)

        FileOpen(1, Server.MapPath("/movie_data/movie_name.txt"), OpenMode.Input)
        gsbnum = CInt(LineInput(1))

        For i = 1 To gsbnum

            chMovieName(i) = LineInput(1)
            enMovieName(i) = LineInput(1)
            theaterNo(i) = LineInput(1)
            showDay1(i) = LineInput(1)
            showDay2(i) = LineInput(1)
            showDay3(i) = LineInput(1)
            showDay4(i) = LineInput(1)
            showDay5(i) = LineInput(1)
            showTime1(i) = LineInput(1)
            showTime2(i) = LineInput(1)
            showTime3(i) = LineInput(1)
            showTime4(i) = LineInput(1)
            showTime5(i) = LineInput(1)
            showTime6(i) = LineInput(1)
            showTime7(i) = LineInput(1)
            showTime8(i) = LineInput(1)

        Next

        FileClose(1)

        For i = 1 To gsbnum - 1

            movie.SelectedIndex = i
            movie.SelectedItem.Value = i
            movie.SelectedItem.Text = chMovieName(i + 1) & Chr(13) & enMovieName(i + 1)

        Next

        movie.SelectedIndex = 0

        Return 0

    End Function


    Public Function day_load(ByVal movieV As Integer) As Integer

        Dim myString = ""
        Dim gsbnum As Integer
        Dim chMovieName(1000), enMovieName(1000), theaterNo(1000), showDay1(1000), showDay2(1000), showDay3(1000), showDay4(1000), showDay5(1000)
        Dim showTime1(1000), showTime2(1000), showTime3(1000), showTime4(1000), showTime5(1000), showTime6(1000), showTime7(1000), showTime8(1000)
        Dim myNum As Integer

        FileOpen(1, Server.MapPath("/movie_data/movie_name.txt"), OpenMode.Input)
        gsbnum = CInt(LineInput(1))

        For i = 1 To gsbnum

            chMovieName(i) = LineInput(1)
            enMovieName(i) = LineInput(1)
            theaterNo(i) = LineInput(1)
            showDay1(i) = LineInput(1)
            showDay2(i) = LineInput(1)
            showDay3(i) = LineInput(1)
            showDay4(i) = LineInput(1)
            showDay5(i) = LineInput(1)
            showTime1(i) = LineInput(1)
            showTime2(i) = LineInput(1)
            showTime3(i) = LineInput(1)
            showTime4(i) = LineInput(1)
            showTime5(i) = LineInput(1)
            showTime6(i) = LineInput(1)
            showTime7(i) = LineInput(1)
            showTime8(i) = LineInput(1)

        Next

        FileClose(1)

        myNum = movieV + 1

        day.Items.Clear()

        day.Items.Insert(0, "請選擇電影的日期")
        day.Items.Insert(1, showDay1(myNum))
        day.Items.Insert(2, showDay2(myNum))
        day.Items.Insert(3, showDay3(myNum))
        day.Items.Insert(4, showDay4(myNum))
        day.Items.Insert(5, showDay5(myNum))


        Return 0

    End Function

    Public Function scene_load(ByVal dayV As Integer) As Integer

        Dim myString = ""
        Dim gsbnum As Integer
        Dim chMovieName(1000), enMovieName(1000), theaterNo(1000), showDay1(1000), showDay2(1000), showDay3(1000), showDay4(1000), showDay5(1000)
        Dim showTime1(1000), showTime2(1000), showTime3(1000), showTime4(1000), showTime5(1000), showTime6(1000), showTime7(1000), showTime8(1000)
        Dim myNum As Integer

        FileOpen(1, Server.MapPath("/movie_data/movie_name.txt"), OpenMode.Input)
        gsbnum = CInt(LineInput(1))

        For i = 1 To gsbnum

            chMovieName(i) = LineInput(1)
            enMovieName(i) = LineInput(1)
            theaterNo(i) = LineInput(1)
            showDay1(i) = LineInput(1)
            showDay2(i) = LineInput(1)
            showDay3(i) = LineInput(1)
            showDay4(i) = LineInput(1)
            showDay5(i) = LineInput(1)
            showTime1(i) = LineInput(1)
            showTime2(i) = LineInput(1)
            showTime3(i) = LineInput(1)
            showTime4(i) = LineInput(1)
            showTime5(i) = LineInput(1)
            showTime6(i) = LineInput(1)
            showTime7(i) = LineInput(1)
            showTime8(i) = LineInput(1)

        Next

        FileClose(1)


        myNum = dayV + 1

        scene.Items.Clear()

        scene.Items.Insert(0, "請選擇電影的場次")
        scene.Items.Insert(1, showTime1(myNum))
        scene.Items.Insert(2, showTime2(myNum))
        scene.Items.Insert(3, showTime3(myNum))
        scene.Items.Insert(4, showTime4(myNum))
        scene.Items.Insert(5, showTime5(myNum))
        scene.Items.Insert(6, showTime6(myNum))
        scene.Items.Insert(7, showTime7(myNum))
        scene.Items.Insert(8, showTime8(myNum))

        Return 0

    End Function


    Protected Sub Page_Load(sender As Object, e As EventArgs)

        Dim movieT As Integer = Application("movieTest")
        Dim movieN As Integer = movie.SelectedIndex
        Dim dayN As Integer = day.SelectedIndex
        Dim sceneN As Integer = scene.SelectedIndex

        If movieN = 0 Then

            movie_load()

            day.SelectedIndex = 0
            scene.SelectedIndex = 0

        End If
        If movieN = 0 And dayN <> 0 And sceneN <> 0 Then

            day.SelectedIndex = 0
            scene.SelectedIndex = 0

        End If

        If movieN = 0 And dayN = 0 And sceneN <> 0 Then

            day.SelectedIndex = 0
            scene.SelectedIndex = 0

        End If

        If Application("movieTest") <> movieN Then

            day.SelectedIndex = 0
            scene.SelectedIndex = 0

        End If

        If movieN <> 0 And dayN = 0 Then

            day_load(movieN)
            scene.SelectedIndex = 0

        End If
        If movieN <> 0 And dayN <> 0 And sceneN = 0 Then

            scene_load(movieN)

        End If

        Application("movieTest") = movieN


    End Sub


    Protected Sub Index_logo_Click(sender As Object, e As ImageClickEventArgs)

        Response.Redirect("movindex.aspx")

    End Sub


    Protected Sub Button1_Click(sender As Object, e As EventArgs)

        Dim movie_sel As Integer = CInt(movie.SelectedIndex)
        Dim day_sel As Integer = CInt(day.SelectedIndex)
        Dim scene_sel As Integer = CInt(scene.SelectedIndex)

        Dim day_text As String = day.SelectedItem.Text
        Dim scene_text As String = scene.SelectedItem.Text

        Application("customer_id_num") = write_customer_number(movie_sel, day_text, scene_text)
        Application("day_num") = day_sel
        Application("scene_num") = scene_sel
        If movie_sel >= 1 AndAlso day_sel >= 1 AndAlso scene_sel >= 1 Then

            Response.Redirect("movbooking.aspx")

        End If


    End Sub

    Protected Sub message_board_Click(sender As Object, e As EventArgs)

        Response.Redirect("chatRoom.aspx")

    End Sub
</script>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
<title>映影城-InMovie</title>
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
      .auto-style1 {
          height: 166px;
          width: 944px;
      }
      .auto-style2 {
          height: 37px;
          width: 210px;
          margin-top: 108px;
          margin-left: 527px;
      }
      .auto-style3 {
          height: 631px;
          width: 758px;
          margin-left: 293px;
      }
      .auto-style4 {
          margin-left: 0px;
      }
      .auto-style5 {
          height: 181px;
          width: 1190px;
      }
      .auto-style6 {
          height: 166px;
      }
      .auto-style7 {
          margin-left: 289px;
      }
      -->
  </style>
</head>
<body style="height: 1549px; margin-top: 0px;">
  <form id="index_form" runat="server">
  <div id="Header" style="margin-top: 0px" class="auto-style6">
      <div style="margin-left: 192px; margin-right: 0px; margin-top: 0px;" class="auto-style1">
          <div style="height: 163px; width: 997px; margin-left: 0px; margin-right: 0px; margin-top: 0px">
              <asp:ImageButton ID="Index_logo" runat="server" Height="145px" ImageUrl="~/images/logo.png" style="margin-top: 10px" Width="181px" OnClick="Index_logo_Click" />
          <asp:Button ID="cinema_intr" runat="server" Height="27px" Text="影城介紹" Width="150px" BackColor="#333333" BorderColor="#333333" BorderStyle="Solid" ForeColor="#FFCC00" style="margin-left: 93px" />
          <asp:Button ID="mov_intr" runat="server" Height="27px" Text="電影介紹" Width="150px" BackColor="#333333" BorderColor="#333333" BorderStyle="Solid" ForeColor="#FFCC00" />
          <asp:Button ID="message_board" runat="server" Height="27px" Text="熱門討論區" Width="150px" BackColor="#333333" BorderColor="#333333" BorderStyle="Solid" ForeColor="#FFCC00" OnClick="message_board_Click" />
          <asp:Button ID="business_board" runat="server" Height="27px" Text="業務專區" Width="150px" BackColor="#333333" BorderColor="#333333" BorderStyle="Solid" ForeColor="#FFCC00" />
          </div>
      </div>
      </div>
      <div id="Body1" style="height: 520px; margin-top: 0px" contenteditable="true">
       </div>
      <div id="Body2" style="height: 864px; margin-top: 0px">
          <br />
          <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
          <div style="margin-top: 55px" class="auto-style3">
        <asp:Panel ID="Panel3" runat="server" Height="421px" Width="524px" BackColor="#CCCCCC" BorderColor="Silver" BorderStyle="Outset" Font-Size="Large" Direction="LeftToRight" HorizontalAlign="Center" style="margin-left: 0px; margin-top: 0px">
            <br />
            <br />
            <br />
            <asp:UpdatePanel ID="movie_Update" runat="server">
                <ContentTemplate>
                    <asp:Label ID="mov_label" runat="server" Font-Names="微軟正黑體" Font-Overline="False" Font-Underline="False" Text=" 電 影 "></asp:Label>
                    <asp:DropDownList ID="movie" runat="server" BackColor="White" Font-Size="Large" Height="35px" style="margin-left: 39px; margin-bottom: 0px" Width="300px" AutoPostBack="True">
                        <asp:ListItem Selected="True" Value="0">請選擇想看的電影</asp:ListItem>
                        <asp:ListItem Value="1">請選擇想看的電影</asp:ListItem>
                        <asp:ListItem Value="2">請選擇想看的電影</asp:ListItem>
                        <asp:ListItem Value="3">請選擇想看的電影</asp:ListItem>
                        <asp:ListItem Value="4">請選擇想看的電影</asp:ListItem>
                        <asp:ListItem Value="5">請選擇想看的電影</asp:ListItem>
                    </asp:DropDownList>
                </ContentTemplate>
            </asp:UpdatePanel>
            <br />
            <br />
            <br />
            <br />
            <asp:UpdatePanel ID="UpdatePanel1" runat="server">
                <ContentTemplate>
                    <asp:Label ID="day_label" runat="server" Font-Names="微軟正黑體" Text="日期"></asp:Label>
                    &nbsp;<asp:DropDownList ID="day" runat="server" AutoPostBack="True" Font-Size="Large" Height="35px" style="margin-left: 39px; margin-bottom: 0px;" Width="300px">
                        <asp:ListItem Value="0">請選擇電影的日期</asp:ListItem>
                    </asp:DropDownList>
                </ContentTemplate>
                <Triggers>
                    <asp:AsyncPostBackTrigger ControlID="movie" EventName="SelectedIndexChanged" />
                </Triggers>
            </asp:UpdatePanel>
            <br />
            <br />
            <br />
            <br />
            <asp:UpdatePanel ID="UpdatePanel2" runat="server">
                <ContentTemplate>
                    <asp:Label ID="sen_label0" runat="server" Font-Names="微軟正黑體" Text=" 場 次"></asp:Label>
                    <asp:DropDownList ID="scene" runat="server" AutoPostBack="True" Font-Size="Large" Height="35px" style="margin-left: 39px; margin-bottom: 0px;" Width="300px">
                        <asp:ListItem Value="0">請選擇電影的場次</asp:ListItem>
                    </asp:DropDownList>
                </ContentTemplate>
                <Triggers>
                    <asp:AsyncPostBackTrigger ControlID="day" EventName="SelectedIndexChanged" />
                </Triggers>
            </asp:UpdatePanel>
            <br />
            <br />
            <br />
            <br />
            <div class="auto-style2">
                <asp:Button ID="next_step_01" runat="server" CssClass="auto-style4" Height="35px" OnClick="Button1_Click" Text="下一步" Width="145px" />
            </div>
            <br />
            <br />
            <br />
            <br />
            <br />
            <br />
            <br />
        </asp:Panel>
          </div>
      </div>
      <div id="footer" style="background-color: #333333; margin-right: auto; margin-left: auto; clip: rect(auto, auto, auto, auto);" class="auto-style5">
          <br />
          <br />
          <br />
          <br />
          <asp:Image ID="wed_id" runat="server" CssClass="auto-style7" Height="77px" ImageUrl="~/images/web_id.jpg" Width="80px" />
          &nbsp;&nbsp;&nbsp;
          <asp:Label ID="Label1" runat="server" ForeColor="White" Text="Copyright © 2017 映電影娛樂 INMOVIES ENTERTAINMENT. All Rights Reserved."></asp:Label>
          <br />
          <br />
          <br />
      </div>
  </form>
</body>
</html>
