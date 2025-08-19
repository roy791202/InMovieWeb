<%@ Page Language="VB" %>

<!DOCTYPE html>

<script runat="server">

    Public Function create_seat() As Integer

        Dim myShow As String = ""
        Dim myString As String = ""
        Dim myseat(50, 50)
        Dim mytheater As String = ""
        Dim myday As String = ""

        For A = 1 To 5

            myday = "day" & A

            For W = 1 To 7

                mytheater = "theater" & W

                For K = 1 To 10

                    myShow = "show" & K

                    FileOpen(2, Server.MapPath("/seat_data/" & myday & "/" & mytheater & "/" & myShow & ".txt"), OpenMode.Output)

                    myString = ""

                    For I = 0 To 14

                        myString = myString & Chr(65 + I) & Chr(13)

                        For J = 1 To 24

                            myseat(I, J) = 0

                            myString = myString & myseat(I, J) & Chr(13)

                            Label1.Text = myString

                        Next

                    Next

                    Print(2, myString)

                    FileClose(2)
                Next
            Next
        Next

        Return 0
    End Function

    Public Function screenings_time(ByVal startTime As Date, ByVal time As Double) As Date()

        Dim swTime As Date = startTime
        Dim mixTime(1000) As Date
        mixTime(0) = startTime

        For i = 1 To 7

            mixTime(i) = DateAdd(DateInterval.Minute, time * i, startTime)

        Next

        Return mixTime

    End Function

    Public Function screenings_day(ByVal startday As Date) As String()

        Dim days As Date
        Dim totleDays(1000) As String

        For i = 0 To 5

            days = DateAdd(DateInterval.Weekday, i, startday)

            totleDays(i) = days

        Next

        Return totleDays

    End Function

    Protected Sub initial_button_Click(sender As Object, e As EventArgs)

        Application("myMovie") = ""
        Application("myTheater") = ""
        Application("myDay") = ""
        Application("myTime") = ""

        Dim myString = ""

        FileOpen(2, Server.MapPath("/movie_data/movie_name.txt"), OpenMode.Output)
        myString = myString & "1" & Chr(13)
        myString = myString & "chMovieName" & Chr(13)
        myString = myString & "enMovieName" & Chr(13)
        myString = myString & "theaterNo" & Chr(13)
        myString = myString & "showDay" & Chr(13)
        myString = myString & "showDay" & Chr(13)
        myString = myString & "showDay" & Chr(13)
        myString = myString & "showDay" & Chr(13)
        myString = myString & "showDay" & Chr(13)
        myString = myString & "showTime" & Chr(13)
        myString = myString & "showTime" & Chr(13)
        myString = myString & "showTime" & Chr(13)
        myString = myString & "showTime" & Chr(13)
        myString = myString & "showTime" & Chr(13)
        myString = myString & "showTime" & Chr(13)
        myString = myString & "showTime" & Chr(13)
        myString = myString & "showTime" & Chr(13)
        myString = myString & "showTime" & Chr(13)

        Print(2, myString)
        FileClose(2)

        initial_tex.Text = "movie_name.txt" & Chr(13) & "has build !"


    End Sub

    Protected Sub add_new_moive_Click(sender As Object, e As EventArgs)

        Dim myString = ""
        Dim gsbnum As Integer
        Dim chMovieName(1000), enMovieName(1000), theaterNo(1000), showDay1(1000), showDay2(1000), showDay3(1000), showDay4(1000), showDay5(1000)
        Dim showTime1(1000), showTime2(1000), showTime3(1000), showTime4(1000), showTime5(1000), showTime6(1000), showTime7(1000), showTime8(1000)
        Dim Day_Text = "#" & show_date.Text & "#"
        Dim show_len As Double = movie_len.Text
        Dim DayN(1000)
        Dim TimeN(1000) As Date

        DayN = screenings_day(Day_Text)
        TimeN = screenings_time(#9:20:0 AM#, CInt(show_len))

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


        chMovieName(gsbnum + 1) = cht_input_box.Text
        enMovieName(gsbnum + 1) = en_input_box.Text
        theaterNo(gsbnum + 1) = theater_list.SelectedItem.Text
        showDay1(gsbnum + 1) = DayN(0)
        showDay2(gsbnum + 1) = DayN(1)
        showDay3(gsbnum + 1) = DayN(2)
        showDay4(gsbnum + 1) = DayN(3)
        showDay5(gsbnum + 1) = DayN(4)
        showTime1(gsbnum + 1) = TimeN(0)
        showTime2(gsbnum + 1) = TimeN(1)
        showTime3(gsbnum + 1) = TimeN(2)
        showTime4(gsbnum + 1) = TimeN(3)
        showTime5(gsbnum + 1) = TimeN(4)
        showTime6(gsbnum + 1) = TimeN(5)
        showTime7(gsbnum + 1) = TimeN(6)
        showTime8(gsbnum + 1) = TimeN(7)

        FileOpen(2, Server.MapPath("/movie_data/movie_name.txt"), OpenMode.Output)

        myString = myString & gsbnum + 1 & Chr(13)

        For i = 1 To gsbnum + 1

            myString = myString & chMovieName(i) & Chr(13)
            myString = myString & enMovieName(i) & Chr(13)
            myString = myString & theaterNo(i) & Chr(13)
            myString = myString & showDay1(i) & Chr(13)
            myString = myString & showDay2(i) & Chr(13)
            myString = myString & showDay3(i) & Chr(13)
            myString = myString & showDay4(i) & Chr(13)
            myString = myString & showDay5(i) & Chr(13)
            myString = myString & showTime1(i) & Chr(13)
            myString = myString & showTime2(i) & Chr(13)
            myString = myString & showTime3(i) & Chr(13)
            myString = myString & showTime4(i) & Chr(13)
            myString = myString & showTime5(i) & Chr(13)
            myString = myString & showTime6(i) & Chr(13)
            myString = myString & showTime7(i) & Chr(13)
            myString = myString & showTime8(i) & Chr(13)

        Next
        Print(2, myString)
        FileClose(2)

        movie_add.Text = " 您新增的電影<" & cht_input_box.Text & ">成功! "
        movie_add0.Text = showDay1(gsbnum + 1) & "<br />" & showDay2(gsbnum + 1) & "<br />" & showDay3(gsbnum + 1) & "<br />" & showDay4(gsbnum + 1) & "<br />" & showDay5(gsbnum + 1)
        movie_add1.Text = showTime1(gsbnum + 1) & "<br />" & showTime2(gsbnum + 1) & "<br />" & showTime3(gsbnum + 1) & "<br />" & showTime4(gsbnum + 1) & "<br />" & showTime5(gsbnum + 1) & "<br />" & showTime6(gsbnum + 1) & "<br />" & showTime7(gsbnum + 1) & "<br />" & showTime8(gsbnum + 1)
    End Sub

    Protected Sub Button1_Click(sender As Object, e As EventArgs)
        create_seat()
    End Sub
</script>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
  <title></title>
    <style type="text/css">
        .auto-style1 {
            height: 850px;
        }
        .auto-style2 {
            height: 889px;
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
        .auto-style3 {
            margin-bottom: 0px;
        }
        .auto-style4 {
            margin-top: 0px;
        }
        </style>
</head>
<body style="height: 979px">
  <form id="form1" runat="server" class="auto-style2">
  <div class="auto-style1" id="header">
  
      <asp:Button ID="initial_button" runat="server" Height="34px" OnClick="initial_button_Click" Text="initial go!" Width="122px" />
  
      <br />
      <br />
      <br />
          <asp:Label ID="initial_tex" runat="server" Text="Label"></asp:Label>
      <br />
      <br />
      <br />
      <hr />
      資料建立區<br />
      <br />
      <br />
      <br />
  
      中文片名: <asp:TextBox ID="cht_input_box" runat="server" Height="19px" Width="206px"></asp:TextBox>
      <br />
      <br />
      英文片名: <asp:TextBox ID="en_input_box" runat="server" CssClass="auto-style3" Height="22px" Width="203px"></asp:TextBox>
      <br />
      <br />
      <br />
      本影城總共有7個廳:
      <asp:DropDownList ID="theater_list" runat="server" CssClass="auto-style4" Height="74px" Width="278px">
          <asp:ListItem Value="0">選擇播放影廳</asp:ListItem>
          <asp:ListItem Value="1">第1廳</asp:ListItem>
          <asp:ListItem Value="2">第2廳</asp:ListItem>
          <asp:ListItem Value="3">第3廳</asp:ListItem>
          <asp:ListItem Value="4">第4廳</asp:ListItem>
          <asp:ListItem Value="5">第5廳</asp:ListItem>
          <asp:ListItem Value="6">第6廳</asp:ListItem>
          <asp:ListItem Value="7">第7廳</asp:ListItem>
      </asp:DropDownList>
      <br />
      <br />
      播映起始日期( 月/日/年 ): <asp:TextBox ID="show_date" runat="server" Height="25px" Width="162px"></asp:TextBox>
      <br />
      <br />
      片長+清場時間(單位:分鐘):
      <asp:TextBox ID="movie_len" runat="server" Height="27px" Width="158px"></asp:TextBox>
      <br />
      <br />
      <asp:Button ID="add_new_moive" runat="server" Height="43px" OnClick="add_new_moive_Click" Text="新增電影" Width="128px" />
      <br />
      <br />
      <asp:Label ID="movie_add" runat="server" Text="Label"></asp:Label>
      <br />
      <br />
      <asp:Label ID="movie_add0" runat="server" Text="Label"></asp:Label>
      <br />
      <br />
      <asp:Label ID="movie_add1" runat="server" Text="Label"></asp:Label>
      <br />
      <br />
  
      <hr />
  
      <asp:Button ID="Button1" runat="server" Height="38px" Text="新增座位表" Width="124px" OnClick="Button1_Click" />
  
      <br />
  
      <asp:Label ID="Label1" runat="server" Text="Label"></asp:Label>
  
      <br />
      <br />
      <br />
      <br />
      <br />
      <asp:Button ID="Button2" runat="server" Height="33px" Text="查看座位" Width="121px" />
      <br />
  
      </div>
      <p>
          &nbsp;</p>
  </form>
</body>
</html>
