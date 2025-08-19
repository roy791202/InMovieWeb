<%@ Page Language="VB" %>

<!DOCTYPE html>

<script runat="server">

    Public Function theater_wrap(theater As String) As Integer

        Dim theater_num As Integer = 0

        If theater = "第1廳" Then

            theater_num = 1

        ElseIf theater = "第2廳" Then

            theater_num = 2

        ElseIf theater = "第3廳" Then

            theater_num = 3

        ElseIf theater = "第4廳" Then

            theater_num = 4

        ElseIf theater = "第5廳" Then

            theater_num = 5

        ElseIf theater = "第6廳" Then

            theater_num = 6

        ElseIf theater = "第7廳" Then

            theater_num = 7

        End If


        Return theater_num

    End Function


    Public Function theater_1_seat(column As Integer, row As Integer) As Button

        Dim my_seat As Button
        Dim my_seat_get()() As Button = New Button(14)() {}
        my_seat_get(0) = {A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24}
        my_seat_get(1) = {B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21, B22, B23, B24}
        my_seat_get(2) = {C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22, C23, C24}
        my_seat_get(3) = {D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15, D16, D17, D18, D19, D20, D21, D22, D23, D24}
        my_seat_get(4) = {E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, E18, E19, E20, E21, E22, E23, E24}
        my_seat_get(5) = {F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, F23, F24}
        my_seat_get(6) = {G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12, G13, G14, G15, G16, G17, G18, G19, G20, G21, G22, G23, G24}
        my_seat_get(7) = {H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16, H17, H18, H19, H20, H21, H22, H23, H24}
        my_seat_get(8) = {I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22, I23, I24}
        my_seat_get(9) = {J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24}
        my_seat_get(10) = {K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, K12, K13, K14, K15, K16, K17, K18, K19, K20, K21, K22, K23, K24}
        my_seat_get(11) = {L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21, L22, L23, L24}
        my_seat_get(12) = {M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24}
        my_seat_get(13) = {N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, N11, N12, N13, N14, N15, N16, N17, N18, N19, N20, N21, N22, N23, N24}
        my_seat_get(14) = {O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15, O16, O17, O18, O19, O20, O21, O22, O23, O24}

        my_seat = my_seat_get(column)(row)

        Return my_seat

    End Function

    Public Function delete_customer_file() As String

        Dim customer_id_num_h As String = mov_id_text.Text

        If System.IO.File.Exists("/custom_data/" & customer_id_num_h & ".txt") = True Then

            System.IO.File.Delete("/custom_data/" & customer_id_num_h & ".txt")

        End If

        Return customer_id_num_h

    End Function

    Public Function check_seat_color(seat As Button) As Integer

        Dim int As Integer

        If seat.BackColor = Drawing.Color.Red Then

            int = 1
        Else
            int = 0
        End If

        Return int
    End Function

    Public Function check_seat_buy() As String

        Dim check_seat(14, 23) As String
        Dim word As String = ""
        Dim check_buy As Integer = 0


        For I = 0 To 14

            For J = 0 To 23

                check_seat(I, J) = (Chr(65 + I)) & "排" & (J + 1) & "號"

                check_buy = check_seat_color(theater_1_seat(I, J))

                If check_buy = 1 Then

                    word = word & check_seat(I, J) & Chr(13)

                End If

            Next
        Next

        Return word
    End Function

    Public Function load_seat(customer_id_num As String) As Integer

        Dim myString As String = ""
        Dim seat_max As Integer = total_ticket.Text
        Dim chMovieName As String = ""
        Dim enMovieName As String = ""
        Dim theater As String = ""
        Dim time As String = ""
        Dim day As String = ""

        Dim myseat(50, 50)
        Dim mytheater As String = "theater"
        Dim myday As String = "day" & day_int.Text
        Dim myShow As String = "show" & time_int.Text

        FileOpen(1, Server.MapPath("/custom_data/" & customer_id_num & ".txt"), OpenMode.Input)

        chMovieName = LineInput(1)
        enMovieName = LineInput(1)
        theater = LineInput(1)
        day = LineInput(1)
        time = LineInput(1)


        FileClose(1)

        mytheater = mytheater & theater_wrap(theater)


        FileOpen(1, Server.MapPath("/seat_data/" & myday & "/" & mytheater & "/" & myShow & ".txt"), OpenMode.Input)

        For I = 0 To 14

            myString = LineInput(1)

            For J = 0 To 23

                myseat(I, J) = LineInput(1)

                If myseat(I, J) = 1 Then

                    theater_1_seat(I, J).BackColor = Drawing.Color.Blue
                    theater_1_seat(I, J).Enabled = 0
                    seat_max = seat_max - 1
                    total_ticket.Text = seat_max
                Else

                    theater_1_seat(I, J).BackColor = Drawing.Color.Silver

                End If

            Next

        Next

        FileClose(1)

        Return 0

    End Function

    Public Function write_seat_data(customer_id_num As String) As Integer

        Dim myString(1000) As String
        Dim myStringWrite As String = ""
        Dim chMovieName As String = ""
        Dim enMovieName As String = ""
        Dim theater As String = ""
        Dim time As String = ""
        Dim day As String = ""

        Dim check_buy As Integer = 0

        Dim myseat(50, 50)
        Dim mytheater As String = "theater"
        Dim myday As String = "day" & day_int.Text
        Dim myShow As String = "show" & time_int.Text

        FileOpen(1, Server.MapPath("/custom_data/" & customer_id_num & ".txt"), OpenMode.Input)

        chMovieName = LineInput(1)
        enMovieName = LineInput(1)
        theater = LineInput(1)
        day = LineInput(1)
        time = LineInput(1)


        FileClose(1)

        mytheater = mytheater & theater_wrap(theater)

        FileOpen(1, Server.MapPath("/seat_data/" & myday & "/" & mytheater & "/" & myShow & ".txt"), OpenMode.Input)

        For I = 0 To 14

            myString(I) = LineInput(1)

            For J = 0 To 23

                myseat(I, J) = LineInput(1)

                check_buy = check_seat_color(theater_1_seat(I, J))

                If check_buy = 1 Then

                    myseat(I, J) = 1

                End If

            Next

        Next

        FileClose(1)

        FileOpen(2, Server.MapPath("/seat_data/" & myday & "/" & mytheater & "/" & myShow & ".txt"), OpenMode.Output)

        For I = 0 To 14

            myStringWrite = myStringWrite & myString(I) & Chr(13)

            For J = 0 To 23

                myStringWrite = myStringWrite & myseat(I, J) & Chr(13)

            Next

        Next

        Print(2, myStringWrite)

        FileClose(2)

        Return 0

    End Function

    Public Function write_custom_data() As Integer

        Dim customer_id_num As String = mov_id_text.Text
        Dim myString As String = ""
        Dim chMovieName As String = ""
        Dim enMovieName As String = ""
        Dim theater As String = ""
        Dim day As String = ""
        Dim time As String = ""
        Dim price_mix As String = ""
        Dim ticket_mix As String = ""
        Dim seat_id As String = check_seat_buy()

        FileOpen(1, Server.MapPath("/custom_data/" & customer_id_num & ".txt"), OpenMode.Input)

        chMovieName = LineInput(1)
        enMovieName = LineInput(1)
        theater = LineInput(1)
        day = LineInput(1)
        time = LineInput(1)
        customer_id_num = LineInput(1)
        price_mix = LineInput(1)
        ticket_mix = LineInput(1)

        FileClose(1)


        FileOpen(2, Server.MapPath("/custom_data/" & customer_id_num & ".txt"), OpenMode.Output)

        myString = myString & chMovieName & Chr(13)
        myString = myString & enMovieName & Chr(13)
        myString = myString & theater & Chr(13)
        myString = myString & day & Chr(13)
        myString = myString & time & Chr(13)
        myString = myString & customer_id_num & Chr(13)
        myString = myString & price_mix & Chr(13)
        myString = myString & ticket_mix & Chr(13)
        myString = myString & seat_id & Chr(13)

        Print(2, myString)
        FileClose(2)



        Return 0

    End Function

    Public Function write_ticket_price_mix(ByVal seat_id As Button, seat_number As Integer) As Button

        If seat_number > 0 Then
            If seat_id.BackColor = Drawing.Color.Silver Then
                seat_id.BackColor = Drawing.Color.Red
                customer_ticket.Text = seat_number - 1
            ElseIf seat_id.BackColor = Drawing.Color.Red Then
                seat_id.BackColor = Drawing.Color.Silver
                customer_ticket.Text = seat_number + 1
            ElseIf seat_id.BackColor = Drawing.Color.Blue Then
                masg_text.Text = "抱歉喔!這個位置已被預訂了!"
            End If
        ElseIf seat_number = 0 And seat_id.BackColor = Drawing.Color.Red Then
            seat_id.BackColor = Drawing.Color.Silver
            customer_ticket.Text = seat_number + 1
        ElseIf seat_number = 0 And seat_id.BackColor = Drawing.Color.Silver Then
            masg_text.Text = "請按下一步完成購票手續!"
        End If


        Return seat_id

    End Function

    Public Function frist_page_load() As Integer

        Dim num As Integer = 0
        Dim customer_id_num_h As String = Application("customer_id_num_seat")
        time_int.Text = Application("scene_num_step2")
        day_int.Text = Application("day_num_step2")
        Dim customer_id As String = ""
        Dim chMovieName As String = ""
        Dim enMovieName As String = ""
        Dim theater As String = ""
        Dim day As String = ""
        Dim time As String = ""
        Dim ticket_price_mix As String = ""
        Dim ticket_mix As String = ""

        FileOpen(1, Server.MapPath("/custom_data/" & customer_id_num_h & ".txt"), OpenMode.Input)

        chMovieName = LineInput(1)
        enMovieName = LineInput(1)
        theater = LineInput(1)
        day = LineInput(1)
        time = LineInput(1)
        customer_id = LineInput(1)
        ticket_price_mix = LineInput(1)
        ticket_mix = LineInput(1)

        FileClose(1)

        mov_ch_text.Text = chMovieName
        mov_en_text.Text = enMovieName
        theater_text.Text = theater
        day_text.Text = day
        time_text.Text = time
        mov_id_text.Text = customer_id_num_h
        customer_ticket.Text = ticket_mix

        Application("frist_load") = 1
        num = Application("frist_load")
        load_seat(mov_id_text.Text)

        Return num

    End Function

    Protected Sub Page_Load(sender As Object, e As EventArgs)

        If Application("frist_load") = 0 Then

            frist_page_load()

        End If

    End Sub

    Protected Sub Index_logo_Click(sender As Object, e As ImageClickEventArgs)
        Response.Redirect("movindex.aspx")
    End Sub

    Protected Sub A1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A1, customer_ticket.Text)
    End Sub
    Protected Sub A2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A2, customer_ticket.Text)
    End Sub
    Protected Sub A3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A3, customer_ticket.Text)
    End Sub
    Protected Sub A4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A4, customer_ticket.Text)
    End Sub
    Protected Sub A5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A5, customer_ticket.Text)
    End Sub
    Protected Sub A6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A6, customer_ticket.Text)
    End Sub
    Protected Sub A7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A7, customer_ticket.Text)
    End Sub
    Protected Sub A8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A8, customer_ticket.Text)
    End Sub
    Protected Sub A9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A9, customer_ticket.Text)
    End Sub
    Protected Sub A10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A10, customer_ticket.Text)
    End Sub
    Protected Sub A11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A11, customer_ticket.Text)
    End Sub
    Protected Sub A12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A12, customer_ticket.Text)
    End Sub
    Protected Sub A13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A13, customer_ticket.Text)
    End Sub
    Protected Sub A14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A14, customer_ticket.Text)
    End Sub
    Protected Sub A15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A15, customer_ticket.Text)
    End Sub
    Protected Sub A16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A16, customer_ticket.Text)
    End Sub
    Protected Sub A17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A17, customer_ticket.Text)
    End Sub
    Protected Sub A18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A18, customer_ticket.Text)
    End Sub
    Protected Sub A19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A19, customer_ticket.Text)
    End Sub
    Protected Sub A20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A20, customer_ticket.Text)
    End Sub
    Protected Sub A21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A21, customer_ticket.Text)
    End Sub
    Protected Sub A22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A22, customer_ticket.Text)
    End Sub
    Protected Sub A23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A23, customer_ticket.Text)
    End Sub
    Protected Sub A24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(A24, customer_ticket.Text)
    End Sub
    Protected Sub B1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B1, customer_ticket.Text)
    End Sub
    Protected Sub B2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B2, customer_ticket.Text)
    End Sub
    Protected Sub B3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B3, customer_ticket.Text)
    End Sub
    Protected Sub B4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B4, customer_ticket.Text)
    End Sub
    Protected Sub B5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B5, customer_ticket.Text)
    End Sub
    Protected Sub B6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B6, customer_ticket.Text)
    End Sub
    Protected Sub B7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B7, customer_ticket.Text)
    End Sub
    Protected Sub B8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B8, customer_ticket.Text)
    End Sub
    Protected Sub B9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B9, customer_ticket.Text)
    End Sub
    Protected Sub B10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B10, customer_ticket.Text)
    End Sub
    Protected Sub B11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B11, customer_ticket.Text)
    End Sub
    Protected Sub B12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B12, customer_ticket.Text)
    End Sub
    Protected Sub B13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B13, customer_ticket.Text)
    End Sub
    Protected Sub B14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B14, customer_ticket.Text)
    End Sub
    Protected Sub B15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B15, customer_ticket.Text)
    End Sub
    Protected Sub B16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B16, customer_ticket.Text)
    End Sub
    Protected Sub B17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B17, customer_ticket.Text)
    End Sub
    Protected Sub B18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B18, customer_ticket.Text)
    End Sub
    Protected Sub B19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B19, customer_ticket.Text)
    End Sub
    Protected Sub B20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B20, customer_ticket.Text)
    End Sub
    Protected Sub B21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B21, customer_ticket.Text)
    End Sub
    Protected Sub B22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B22, customer_ticket.Text)
    End Sub
    Protected Sub B23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B23, customer_ticket.Text)
    End Sub
    Protected Sub B24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(B24, customer_ticket.Text)
    End Sub
    Protected Sub C1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C1, customer_ticket.Text)
    End Sub
    Protected Sub C2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C2, customer_ticket.Text)
    End Sub
    Protected Sub C3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C3, customer_ticket.Text)
    End Sub
    Protected Sub C4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C4, customer_ticket.Text)
    End Sub
    Protected Sub C5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C5, customer_ticket.Text)
    End Sub
    Protected Sub C6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C6, customer_ticket.Text)
    End Sub
    Protected Sub C7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C7, customer_ticket.Text)
    End Sub
    Protected Sub C8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C8, customer_ticket.Text)
    End Sub
    Protected Sub C9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C9, customer_ticket.Text)
    End Sub
    Protected Sub C10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C10, customer_ticket.Text)
    End Sub
    Protected Sub C11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C11, customer_ticket.Text)
    End Sub
    Protected Sub C12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C12, customer_ticket.Text)
    End Sub
    Protected Sub C13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C13, customer_ticket.Text)
    End Sub
    Protected Sub C14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C14, customer_ticket.Text)
    End Sub
    Protected Sub C15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C15, customer_ticket.Text)
    End Sub
    Protected Sub C16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C16, customer_ticket.Text)
    End Sub
    Protected Sub C17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C17, customer_ticket.Text)
    End Sub
    Protected Sub C18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C18, customer_ticket.Text)
    End Sub
    Protected Sub C19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C19, customer_ticket.Text)
    End Sub
    Protected Sub C20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C20, customer_ticket.Text)
    End Sub
    Protected Sub C21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C21, customer_ticket.Text)
    End Sub
    Protected Sub C22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C22, customer_ticket.Text)
    End Sub
    Protected Sub C23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C23, customer_ticket.Text)
    End Sub
    Protected Sub C24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(C24, customer_ticket.Text)
    End Sub
    Protected Sub D1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D1, customer_ticket.Text)
    End Sub
    Protected Sub D2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D2, customer_ticket.Text)
    End Sub
    Protected Sub D3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D3, customer_ticket.Text)
    End Sub
    Protected Sub D4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D4, customer_ticket.Text)
    End Sub
    Protected Sub D5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D5, customer_ticket.Text)
    End Sub
    Protected Sub D6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D6, customer_ticket.Text)
    End Sub
    Protected Sub D7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D7, customer_ticket.Text)
    End Sub
    Protected Sub D8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D8, customer_ticket.Text)
    End Sub
    Protected Sub D9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D9, customer_ticket.Text)
    End Sub
    Protected Sub D10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D10, customer_ticket.Text)
    End Sub
    Protected Sub D11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D11, customer_ticket.Text)
    End Sub
    Protected Sub D12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D12, customer_ticket.Text)
    End Sub
    Protected Sub D13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D13, customer_ticket.Text)
    End Sub
    Protected Sub D14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D14, customer_ticket.Text)
    End Sub
    Protected Sub D15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D15, customer_ticket.Text)
    End Sub
    Protected Sub D16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D16, customer_ticket.Text)
    End Sub
    Protected Sub D17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D17, customer_ticket.Text)
    End Sub
    Protected Sub D18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D18, customer_ticket.Text)
    End Sub
    Protected Sub D19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D19, customer_ticket.Text)
    End Sub
    Protected Sub D20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D20, customer_ticket.Text)
    End Sub
    Protected Sub D21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D21, customer_ticket.Text)
    End Sub
    Protected Sub D22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D22, customer_ticket.Text)
    End Sub
    Protected Sub D23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D23, customer_ticket.Text)
    End Sub
    Protected Sub D24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(D24, customer_ticket.Text)
    End Sub
    Protected Sub E1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E1, customer_ticket.Text)
    End Sub
    Protected Sub E2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E2, customer_ticket.Text)
    End Sub
    Protected Sub E3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E3, customer_ticket.Text)
    End Sub
    Protected Sub E4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E4, customer_ticket.Text)
    End Sub
    Protected Sub E5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E5, customer_ticket.Text)
    End Sub
    Protected Sub E6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E6, customer_ticket.Text)
    End Sub
    Protected Sub E7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E7, customer_ticket.Text)
    End Sub
    Protected Sub E8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E8, customer_ticket.Text)
    End Sub
    Protected Sub E9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E9, customer_ticket.Text)
    End Sub
    Protected Sub E10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E10, customer_ticket.Text)
    End Sub
    Protected Sub E11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E11, customer_ticket.Text)
    End Sub
    Protected Sub E12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E12, customer_ticket.Text)
    End Sub
    Protected Sub E13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E13, customer_ticket.Text)
    End Sub
    Protected Sub E14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E14, customer_ticket.Text)
    End Sub
    Protected Sub E15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E15, customer_ticket.Text)
    End Sub
    Protected Sub E16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E16, customer_ticket.Text)
    End Sub
    Protected Sub E17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E17, customer_ticket.Text)
    End Sub
    Protected Sub E18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E18, customer_ticket.Text)
    End Sub
    Protected Sub E19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E19, customer_ticket.Text)
    End Sub
    Protected Sub E20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E20, customer_ticket.Text)
    End Sub
    Protected Sub E21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E21, customer_ticket.Text)
    End Sub
    Protected Sub E22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E22, customer_ticket.Text)
    End Sub
    Protected Sub E23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E23, customer_ticket.Text)
    End Sub
    Protected Sub E24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(E24, customer_ticket.Text)
    End Sub
    Protected Sub F1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F1, customer_ticket.Text)
    End Sub
    Protected Sub F2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F2, customer_ticket.Text)
    End Sub
    Protected Sub F3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F3, customer_ticket.Text)
    End Sub
    Protected Sub F4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F4, customer_ticket.Text)
    End Sub
    Protected Sub F5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F5, customer_ticket.Text)
    End Sub
    Protected Sub F6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F6, customer_ticket.Text)
    End Sub
    Protected Sub F7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F7, customer_ticket.Text)
    End Sub
    Protected Sub F8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F8, customer_ticket.Text)
    End Sub
    Protected Sub F9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F9, customer_ticket.Text)
    End Sub
    Protected Sub F10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F10, customer_ticket.Text)
    End Sub
    Protected Sub F11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F11, customer_ticket.Text)
    End Sub
    Protected Sub F12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F12, customer_ticket.Text)
    End Sub
    Protected Sub F13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F13, customer_ticket.Text)
    End Sub
    Protected Sub F14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F14, customer_ticket.Text)
    End Sub
    Protected Sub F15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F15, customer_ticket.Text)
    End Sub
    Protected Sub F16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F16, customer_ticket.Text)
    End Sub
    Protected Sub F17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F17, customer_ticket.Text)
    End Sub
    Protected Sub F18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F18, customer_ticket.Text)
    End Sub
    Protected Sub F19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F19, customer_ticket.Text)
    End Sub
    Protected Sub F20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F20, customer_ticket.Text)
    End Sub
    Protected Sub F21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F21, customer_ticket.Text)
    End Sub
    Protected Sub F22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F22, customer_ticket.Text)
    End Sub
    Protected Sub F23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F23, customer_ticket.Text)
    End Sub
    Protected Sub F24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(F24, customer_ticket.Text)
    End Sub
    Protected Sub G1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G1, customer_ticket.Text)
    End Sub
    Protected Sub G2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G2, customer_ticket.Text)
    End Sub
    Protected Sub G3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G3, customer_ticket.Text)
    End Sub
    Protected Sub G4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G4, customer_ticket.Text)
    End Sub
    Protected Sub G5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G5, customer_ticket.Text)
    End Sub
    Protected Sub G6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G6, customer_ticket.Text)
    End Sub
    Protected Sub G7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G7, customer_ticket.Text)
    End Sub
    Protected Sub G8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G8, customer_ticket.Text)
    End Sub
    Protected Sub G9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G9, customer_ticket.Text)
    End Sub
    Protected Sub G10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G10, customer_ticket.Text)
    End Sub
    Protected Sub G11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G11, customer_ticket.Text)
    End Sub
    Protected Sub G12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G12, customer_ticket.Text)
    End Sub
    Protected Sub G13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G13, customer_ticket.Text)
    End Sub
    Protected Sub G14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G14, customer_ticket.Text)
    End Sub
    Protected Sub G15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G15, customer_ticket.Text)
    End Sub
    Protected Sub G16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G16, customer_ticket.Text)
    End Sub
    Protected Sub G17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G17, customer_ticket.Text)
    End Sub
    Protected Sub G18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G18, customer_ticket.Text)
    End Sub
    Protected Sub G19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G19, customer_ticket.Text)
    End Sub
    Protected Sub G20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G20, customer_ticket.Text)
    End Sub
    Protected Sub G21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G21, customer_ticket.Text)
    End Sub
    Protected Sub G22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G22, customer_ticket.Text)
    End Sub
    Protected Sub G23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G23, customer_ticket.Text)
    End Sub
    Protected Sub G24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(G24, customer_ticket.Text)
    End Sub
    Protected Sub H1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H1, customer_ticket.Text)
    End Sub
    Protected Sub H2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H2, customer_ticket.Text)
    End Sub
    Protected Sub H3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H3, customer_ticket.Text)
    End Sub
    Protected Sub H4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H4, customer_ticket.Text)
    End Sub
    Protected Sub H5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H5, customer_ticket.Text)
    End Sub
    Protected Sub H6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H6, customer_ticket.Text)
    End Sub
    Protected Sub H7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H7, customer_ticket.Text)
    End Sub
    Protected Sub H8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H8, customer_ticket.Text)
    End Sub
    Protected Sub H9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H9, customer_ticket.Text)
    End Sub
    Protected Sub H10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H10, customer_ticket.Text)
    End Sub
    Protected Sub H11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H11, customer_ticket.Text)
    End Sub
    Protected Sub H12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H12, customer_ticket.Text)
    End Sub
    Protected Sub H13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H13, customer_ticket.Text)
    End Sub
    Protected Sub H14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H14, customer_ticket.Text)
    End Sub
    Protected Sub H15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H15, customer_ticket.Text)
    End Sub
    Protected Sub H16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H16, customer_ticket.Text)
    End Sub
    Protected Sub H17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H17, customer_ticket.Text)
    End Sub
    Protected Sub H18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H18, customer_ticket.Text)
    End Sub
    Protected Sub H19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H19, customer_ticket.Text)
    End Sub
    Protected Sub H20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H20, customer_ticket.Text)
    End Sub
    Protected Sub H21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H21, customer_ticket.Text)
    End Sub
    Protected Sub H22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H22, customer_ticket.Text)
    End Sub
    Protected Sub H23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H23, customer_ticket.Text)
    End Sub

    Protected Sub H24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(H24, customer_ticket.Text)
    End Sub

    Protected Sub I1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I1, customer_ticket.Text)
    End Sub
    Protected Sub I2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I2, customer_ticket.Text)
    End Sub
    Protected Sub I3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I3, customer_ticket.Text)
    End Sub
    Protected Sub I4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I4, customer_ticket.Text)
    End Sub
    Protected Sub I5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I5, customer_ticket.Text)
    End Sub
    Protected Sub I6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I6, customer_ticket.Text)
    End Sub
    Protected Sub I7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I7, customer_ticket.Text)
    End Sub
    Protected Sub I8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I8, customer_ticket.Text)
    End Sub
    Protected Sub I9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I9, customer_ticket.Text)
    End Sub
    Protected Sub I10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I10, customer_ticket.Text)
    End Sub
    Protected Sub I11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I11, customer_ticket.Text)
    End Sub
    Protected Sub I12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I12, customer_ticket.Text)
    End Sub
    Protected Sub I13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I13, customer_ticket.Text)
    End Sub
    Protected Sub I14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I14, customer_ticket.Text)
    End Sub
    Protected Sub I15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I15, customer_ticket.Text)
    End Sub
    Protected Sub I16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I16, customer_ticket.Text)
    End Sub
    Protected Sub I17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I17, customer_ticket.Text)
    End Sub
    Protected Sub I18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I18, customer_ticket.Text)
    End Sub
    Protected Sub I19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I19, customer_ticket.Text)
    End Sub
    Protected Sub I20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I20, customer_ticket.Text)
    End Sub
    Protected Sub I21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I21, customer_ticket.Text)
    End Sub
    Protected Sub I22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I22, customer_ticket.Text)
    End Sub
    Protected Sub I23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I23, customer_ticket.Text)
    End Sub
    Protected Sub I24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(I24, customer_ticket.Text)
    End Sub

    Protected Sub J1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J1, customer_ticket.Text)
    End Sub
    Protected Sub J2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J2, customer_ticket.Text)
    End Sub
    Protected Sub J3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J3, customer_ticket.Text)
    End Sub
    Protected Sub J4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J4, customer_ticket.Text)
    End Sub
    Protected Sub J5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J5, customer_ticket.Text)
    End Sub
    Protected Sub J6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J6, customer_ticket.Text)
    End Sub
    Protected Sub J7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J7, customer_ticket.Text)
    End Sub
    Protected Sub J8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J8, customer_ticket.Text)
    End Sub
    Protected Sub J9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J9, customer_ticket.Text)
    End Sub
    Protected Sub J10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J10, customer_ticket.Text)
    End Sub
    Protected Sub J11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J11, customer_ticket.Text)
    End Sub
    Protected Sub J12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J12, customer_ticket.Text)
    End Sub
    Protected Sub J13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J13, customer_ticket.Text)
    End Sub
    Protected Sub J14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J14, customer_ticket.Text)
    End Sub
    Protected Sub J15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J15, customer_ticket.Text)
    End Sub
    Protected Sub J16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J16, customer_ticket.Text)
    End Sub
    Protected Sub J17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J17, customer_ticket.Text)
    End Sub
    Protected Sub J18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J18, customer_ticket.Text)
    End Sub
    Protected Sub J19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J19, customer_ticket.Text)
    End Sub
    Protected Sub J20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J20, customer_ticket.Text)
    End Sub
    Protected Sub J21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J21, customer_ticket.Text)
    End Sub
    Protected Sub J22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J22, customer_ticket.Text)
    End Sub
    Protected Sub J23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J23, customer_ticket.Text)
    End Sub
    Protected Sub J24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(J24, customer_ticket.Text)
    End Sub
    Protected Sub K1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K1, customer_ticket.Text)
    End Sub
    Protected Sub K2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K2, customer_ticket.Text)
    End Sub
    Protected Sub K3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K3, customer_ticket.Text)
    End Sub
    Protected Sub K4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K4, customer_ticket.Text)
    End Sub
    Protected Sub K5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K5, customer_ticket.Text)
    End Sub
    Protected Sub K6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K6, customer_ticket.Text)
    End Sub
    Protected Sub K7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K7, customer_ticket.Text)
    End Sub
    Protected Sub K8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K8, customer_ticket.Text)
    End Sub
    Protected Sub K9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K9, customer_ticket.Text)
    End Sub
    Protected Sub K10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K10, customer_ticket.Text)
    End Sub
    Protected Sub K11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K11, customer_ticket.Text)
    End Sub
    Protected Sub K12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K12, customer_ticket.Text)
    End Sub
    Protected Sub K13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K13, customer_ticket.Text)
    End Sub
    Protected Sub K14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K14, customer_ticket.Text)
    End Sub
    Protected Sub K15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K15, customer_ticket.Text)
    End Sub
    Protected Sub K16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K16, customer_ticket.Text)
    End Sub
    Protected Sub K17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K17, customer_ticket.Text)
    End Sub
    Protected Sub K18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K18, customer_ticket.Text)
    End Sub
    Protected Sub K19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K19, customer_ticket.Text)
    End Sub
    Protected Sub K20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K20, customer_ticket.Text)
    End Sub
    Protected Sub K21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K21, customer_ticket.Text)
    End Sub
    Protected Sub K22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K22, customer_ticket.Text)
    End Sub
    Protected Sub K23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K23, customer_ticket.Text)
    End Sub
    Protected Sub K24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(K24, customer_ticket.Text)
    End Sub
    Protected Sub L1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L1, customer_ticket.Text)
    End Sub
    Protected Sub L2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L2, customer_ticket.Text)
    End Sub
    Protected Sub L3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L3, customer_ticket.Text)
    End Sub
    Protected Sub L4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L4, customer_ticket.Text)
    End Sub
    Protected Sub L5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L5, customer_ticket.Text)
    End Sub
    Protected Sub L6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L6, customer_ticket.Text)
    End Sub
    Protected Sub L7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L7, customer_ticket.Text)
    End Sub
    Protected Sub L8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L8, customer_ticket.Text)
    End Sub
    Protected Sub L9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L9, customer_ticket.Text)
    End Sub
    Protected Sub L10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L10, customer_ticket.Text)
    End Sub
    Protected Sub L11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L11, customer_ticket.Text)
    End Sub
    Protected Sub L12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L12, customer_ticket.Text)
    End Sub
    Protected Sub L13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L13, customer_ticket.Text)
    End Sub
    Protected Sub L14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L14, customer_ticket.Text)
    End Sub
    Protected Sub L15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L15, customer_ticket.Text)
    End Sub
    Protected Sub L16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L16, customer_ticket.Text)
    End Sub
    Protected Sub L17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L17, customer_ticket.Text)
    End Sub
    Protected Sub L18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L18, customer_ticket.Text)
    End Sub
    Protected Sub L19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L19, customer_ticket.Text)
    End Sub
    Protected Sub L20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L20, customer_ticket.Text)
    End Sub
    Protected Sub L21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L21, customer_ticket.Text)
    End Sub
    Protected Sub L22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L22, customer_ticket.Text)
    End Sub
    Protected Sub L23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L23, customer_ticket.Text)
    End Sub
    Protected Sub L24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(L24, customer_ticket.Text)
    End Sub
    Protected Sub M1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M1, customer_ticket.Text)
    End Sub
    Protected Sub M2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M2, customer_ticket.Text)
    End Sub
    Protected Sub M3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M3, customer_ticket.Text)
    End Sub
    Protected Sub M4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M4, customer_ticket.Text)
    End Sub
    Protected Sub M5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M5, customer_ticket.Text)
    End Sub
    Protected Sub M6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M6, customer_ticket.Text)
    End Sub
    Protected Sub M7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M7, customer_ticket.Text)
    End Sub
    Protected Sub M8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M8, customer_ticket.Text)
    End Sub
    Protected Sub M9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M9, customer_ticket.Text)
    End Sub
    Protected Sub M10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M10, customer_ticket.Text)
    End Sub
    Protected Sub M11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M11, customer_ticket.Text)
    End Sub
    Protected Sub M12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M12, customer_ticket.Text)
    End Sub
    Protected Sub M13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M13, customer_ticket.Text)
    End Sub
    Protected Sub M14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M14, customer_ticket.Text)
    End Sub
    Protected Sub M15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M15, customer_ticket.Text)
    End Sub
    Protected Sub M16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M16, customer_ticket.Text)
    End Sub
    Protected Sub M17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M17, customer_ticket.Text)
    End Sub
    Protected Sub M18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M18, customer_ticket.Text)
    End Sub
    Protected Sub M19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M19, customer_ticket.Text)
    End Sub
    Protected Sub M20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M20, customer_ticket.Text)
    End Sub
    Protected Sub M21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M21, customer_ticket.Text)
    End Sub
    Protected Sub M22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M22, customer_ticket.Text)
    End Sub
    Protected Sub M23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M23, customer_ticket.Text)
    End Sub
    Protected Sub M24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(M24, customer_ticket.Text)
    End Sub
    Protected Sub N1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N1, customer_ticket.Text)
    End Sub
    Protected Sub N2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N2, customer_ticket.Text)
    End Sub
    Protected Sub N3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N3, customer_ticket.Text)
    End Sub
    Protected Sub N4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N4, customer_ticket.Text)
    End Sub
    Protected Sub N5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N5, customer_ticket.Text)
    End Sub
    Protected Sub N6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N6, customer_ticket.Text)
    End Sub
    Protected Sub N7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N7, customer_ticket.Text)
    End Sub
    Protected Sub N8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N8, customer_ticket.Text)
    End Sub
    Protected Sub N9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N9, customer_ticket.Text)
    End Sub
    Protected Sub N10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N10, customer_ticket.Text)
    End Sub
    Protected Sub N11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N11, customer_ticket.Text)
    End Sub
    Protected Sub N12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N12, customer_ticket.Text)
    End Sub
    Protected Sub N13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N13, customer_ticket.Text)
    End Sub
    Protected Sub N14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N14, customer_ticket.Text)
    End Sub
    Protected Sub N15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N15, customer_ticket.Text)
    End Sub
    Protected Sub N16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N16, customer_ticket.Text)
    End Sub
    Protected Sub N17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N17, customer_ticket.Text)
    End Sub
    Protected Sub N18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N18, customer_ticket.Text)
    End Sub
    Protected Sub N19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N19, customer_ticket.Text)
    End Sub
    Protected Sub N20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N20, customer_ticket.Text)
    End Sub
    Protected Sub N21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N21, customer_ticket.Text)
    End Sub
    Protected Sub N22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N22, customer_ticket.Text)
    End Sub
    Protected Sub N23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N23, customer_ticket.Text)
    End Sub
    Protected Sub N24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(N24, customer_ticket.Text)
    End Sub

    Protected Sub O1_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O1, customer_ticket.Text)
    End Sub
    Protected Sub O2_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O2, customer_ticket.Text)
    End Sub
    Protected Sub O3_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O3, customer_ticket.Text)
    End Sub
    Protected Sub O4_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O4, customer_ticket.Text)
    End Sub
    Protected Sub O5_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O5, customer_ticket.Text)
    End Sub
    Protected Sub O6_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O6, customer_ticket.Text)
    End Sub
    Protected Sub O7_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O7, customer_ticket.Text)
    End Sub
    Protected Sub O8_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O8, customer_ticket.Text)
    End Sub
    Protected Sub O9_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O9, customer_ticket.Text)
    End Sub
    Protected Sub O10_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O10, customer_ticket.Text)
    End Sub
    Protected Sub O11_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O11, customer_ticket.Text)
    End Sub
    Protected Sub O12_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O12, customer_ticket.Text)
    End Sub
    Protected Sub O13_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O13, customer_ticket.Text)
    End Sub
    Protected Sub O14_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O14, customer_ticket.Text)
    End Sub
    Protected Sub O15_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O15, customer_ticket.Text)
    End Sub
    Protected Sub O16_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O16, customer_ticket.Text)
    End Sub
    Protected Sub O17_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O17, customer_ticket.Text)
    End Sub
    Protected Sub O18_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O18, customer_ticket.Text)
    End Sub
    Protected Sub O19_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O19, customer_ticket.Text)
    End Sub
    Protected Sub O20_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O20, customer_ticket.Text)
    End Sub
    Protected Sub O21_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O21, customer_ticket.Text)
    End Sub
    Protected Sub O22_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O22, customer_ticket.Text)
    End Sub
    Protected Sub O23_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O23, customer_ticket.Text)
    End Sub
    Protected Sub O24_Click(sender As Object, e As EventArgs)
        write_ticket_price_mix(O24, customer_ticket.Text)
    End Sub

    Protected Sub ticker_price_back_Click(sender As Object, e As EventArgs)
        delete_customer_file()
        Response.Redirect("movindex.aspx")
    End Sub

    Protected Sub ticker_price_next_Click(sender As Object, e As EventArgs)

        Application("result_load") = 0
        Application("result_id") = mov_id_text.Text
        write_seat_data(mov_id_text.Text)
        write_custom_data()
        Response.Redirect("movresult.aspx")

    End Sub
</script>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
  <title>映影城-InMovie-劃位區</title>
    <style type="text/css">
        .auto-style1 {
            height: 166px;
        }
        .auto-style2 {
            width: 45%;
            height: 170px;
            margin-left: 305px;
        }
        .auto-style3 {
            width: 285px;
        }
        .auto-style4 {
            width: 155px;
        }
        .auto-style5 {
            height: 174px;
        }
        .auto-style6 {
            height: 24px;
        }
        .auto-style12 {
            width: 86%;
            height: 649px;
            margin-left: 91px;
        }
        .auto-style13 {
            width: 31px;
        }
        .auto-style14 {
            width: 30px;
        }
        .auto-style15 {
            width: 28px;
        }
        .auto-style16 {
            width: 27px;
        }
        .auto-style17 {
            width: 32px;
        }
        .auto-style18 {
            width: 29px;
        }
        .auto-style20 {
            height: 22px;
        }
        .auto-style21 {
            width: 31px;
            height: 22px;
        }
        .auto-style22 {
            width: 30px;
            height: 22px;
        }
        .auto-style23 {
            width: 28px;
            height: 22px;
        }
        .auto-style24 {
            width: 27px;
            height: 22px;
        }
        .auto-style25 {
            width: 32px;
            height: 22px;
        }
        .auto-style26 {
            width: 29px;
            height: 22px;
        }
        .auto-style27 {
            height: 18px;
        }
        .auto-style28 {
            width: 31px;
            height: 18px;
        }
        .auto-style29 {
            width: 30px;
            height: 18px;
        }
        .auto-style30 {
            width: 28px;
            height: 18px;
        }
        .auto-style31 {
            width: 27px;
            height: 18px;
        }
        .auto-style32 {
            width: 32px;
            height: 18px;
        }
        .auto-style33 {
            width: 29px;
            height: 18px;
        }
        .auto-style40 {
            width: 15px;
        }
        .auto-style41 {
            width: 15px;
            height: 22px;
        }
        .auto-style42 {
            width: 15px;
            height: 18px;
        }
        .auto-style44 {
            width: 111px;
            height: 22px;
        }
        .auto-style45 {
            width: 111px;
            height: 18px;
        }
        .auto-style57 {
            width: 111px;
            height: 21px;
        }
        .auto-style58 {
            width: 31px;
            height: 21px;
        }
        .auto-style59 {
            width: 30px;
            height: 21px;
        }
        .auto-style60 {
            width: 28px;
            height: 21px;
        }
        .auto-style61 {
            width: 27px;
            height: 21px;
        }
        .auto-style62 {
            width: 32px;
            height: 21px;
        }
        .auto-style63 {
            width: 29px;
            height: 21px;
        }
        .auto-style64 {
            width: 15px;
            height: 21px;
        }
        .auto-style65 {
            height: 21px;
        }
        .auto-style67 {
            width: 111px;
            height: 14px;
        }
        .auto-style68 {
            width: 31px;
            height: 14px;
        }
        .auto-style69 {
            width: 30px;
            height: 14px;
        }
        .auto-style70 {
            width: 28px;
            height: 14px;
        }
        .auto-style71 {
            width: 27px;
            height: 14px;
        }
        .auto-style72 {
            width: 32px;
            height: 14px;
        }
        .auto-style73 {
            width: 29px;
            height: 14px;
        }
        .auto-style74 {
            width: 15px;
            height: 14px;
        }
        .auto-style75 {
            height: 14px;
        }
        .auto-style77 {
            width: 111px;
            height: 2px;
        }
        .auto-style78 {
            width: 31px;
            height: 2px;
        }
        .auto-style79 {
            width: 30px;
            height: 2px;
        }
        .auto-style80 {
            width: 28px;
            height: 2px;
        }
        .auto-style81 {
            width: 27px;
            height: 2px;
        }
        .auto-style82 {
            width: 32px;
            height: 2px;
        }
        .auto-style83 {
            width: 29px;
            height: 2px;
        }
        .auto-style84 {
            width: 15px;
            height: 2px;
        }
        .auto-style85 {
            height: 2px;
        }
        .auto-style87 {
            width: 111px;
            height: 8px;
        }
        .auto-style88 {
            width: 31px;
            height: 8px;
        }
        .auto-style89 {
            width: 30px;
            height: 8px;
        }
        .auto-style90 {
            width: 28px;
            height: 8px;
        }
        .auto-style91 {
            width: 27px;
            height: 8px;
        }
        .auto-style92 {
            width: 32px;
            height: 8px;
        }
        .auto-style93 {
            width: 29px;
            height: 8px;
        }
        .auto-style94 {
            width: 15px;
            height: 8px;
        }
        .auto-style95 {
            height: 8px;
        }
        .auto-style107 {
            width: 111px;
            height: 20px;
        }
        .auto-style111 {
            width: 27px;
            height: 20px;
        }
        .auto-style114 {
            width: 15px;
            height: 20px;
        }
        .auto-style118 {
            margin-left: 159px;
            margin-right: 0px;
        }
        .auto-style119 {
            width: 111px;
            height: 31px;
        }
        .auto-style120 {
            width: 31px;
            height: 31px;
        }
        .auto-style121 {
            width: 30px;
            height: 31px;
        }
        .auto-style122 {
            width: 28px;
            height: 31px;
        }
        .auto-style123 {
            width: 27px;
            height: 31px;
        }
        .auto-style124 {
            width: 32px;
            height: 31px;
        }
        .auto-style125 {
            width: 29px;
            height: 31px;
        }
        .auto-style126 {
            width: 15px;
            height: 31px;
        }
        .auto-style127 {
            height: 31px;
        }
        .auto-style129 {
            width: 111px;
            height: 38px;
        }
        .auto-style130 {
            width: 31px;
            height: 38px;
        }
        .auto-style131 {
            width: 30px;
            height: 38px;
        }
        .auto-style132 {
            width: 28px;
            height: 38px;
        }
        .auto-style133 {
            width: 27px;
            height: 38px;
        }
        .auto-style134 {
            width: 32px;
            height: 38px;
        }
        .auto-style135 {
            width: 29px;
            height: 38px;
        }
        .auto-style136 {
            width: 15px;
            height: 38px;
        }
        .auto-style137 {
            height: 38px;
        }
        .auto-style139 {
            width: 111px;
            height: 39px;
        }
        .auto-style140 {
            width: 31px;
            height: 39px;
        }
        .auto-style141 {
            width: 30px;
            height: 39px;
        }
        .auto-style142 {
            width: 28px;
            height: 39px;
        }
        .auto-style143 {
            width: 27px;
            height: 39px;
        }
        .auto-style144 {
            width: 32px;
            height: 39px;
        }
        .auto-style145 {
            width: 29px;
            height: 39px;
        }
        .auto-style146 {
            width: 15px;
            height: 39px;
        }
        .auto-style147 {
            height: 39px;
        }
        .auto-style149 {
            width: 31px;
            height: 20px;
        }
        .auto-style150 {
            width: 30px;
            height: 20px;
        }
        .auto-style151 {
            width: 28px;
            height: 20px;
        }
        .auto-style152 {
            width: 32px;
            height: 20px;
        }
        .auto-style153 {
            width: 29px;
            height: 20px;
        }
        .auto-style154 {
            height: 20px;
        }
        .auto-style155 {
            width: 285px;
            height: 35px;
        }
        .auto-style156 {
            width: 155px;
            height: 35px;
        }
        .auto-style157 {
            height: 35px;
        }
        .auto-style158 {
            width: 285px;
            height: 47px;
        }
        .auto-style159 {
            width: 155px;
            height: 47px;
        }
        .auto-style160 {
            height: 47px;
        }
        .auto-style174 {
            height: 89px;
        }
        .auto-style175 {
            width: 15px;
            height: 22px;
            margin-left: 779px;
        }
        .auto-style176 {
            margin-left: 19px;
        }
        .auto-style177 {
            width: 39px;
        }
        .auto-style178 {
            width: 39px;
            height: 2px;
        }
        .auto-style179 {
            width: 39px;
            height: 14px;
        }
        .auto-style180 {
            width: 39px;
            height: 21px;
        }
        .auto-style181 {
            width: 39px;
            height: 8px;
        }
        .auto-style182 {
            width: 39px;
            height: 22px;
        }
        .auto-style183 {
            width: 39px;
            height: 31px;
        }
        .auto-style184 {
            width: 39px;
            height: 18px;
        }
        .auto-style185 {
            width: 39px;
            height: 38px;
        }
        .auto-style186 {
            width: 39px;
            height: 39px;
        }
        .auto-style187 {
            width: 39px;
            height: 20px;
        }
        .auto-style188 {
            width: 113px;
        }
        .auto-style189 {
            width: 113px;
            height: 2px;
        }
        .auto-style190 {
            width: 113px;
            height: 14px;
        }
        .auto-style191 {
            width: 113px;
            height: 21px;
        }
        .auto-style192 {
            width: 113px;
            height: 8px;
        }
        .auto-style193 {
            width: 113px;
            height: 22px;
        }
        .auto-style194 {
            width: 113px;
            height: 31px;
        }
        .auto-style195 {
            width: 113px;
            height: 18px;
        }
        .auto-style196 {
            width: 113px;
            height: 38px;
        }
        .auto-style197 {
            width: 113px;
            height: 39px;
        }
        .auto-style198 {
            width: 113px;
            height: 20px;
        }
        .auto-style199 {
            width: 111px;
        }
        .auto-style200 {
            height: 177px;
        }
        .auto-style201 {
            height: 89px;
            width: 1171px;
        }
      .auto-style7 {
          margin-left: 289px;
      }
      </style>
</head>
<body style="height: 1488px">
  <form id="form1" runat="server">
  <div id="header" class="auto-style1" style="background-image: none; background-color: #333333;">
  
              <asp:ImageButton ID="Index_logo" runat="server" Height="145px" ImageUrl="~/images/logo.png" style="margin-top: 10px" Width="181px" OnClick="Index_logo_Click" CssClass="auto-style118" />
  
  </div>
      <div>
          <asp:SiteMapPath ID="SiteMapPath1" runat="server">
          </asp:SiteMapPath>
      </div>
      <div id="center1" class="auto-style5">
          <table id="table_A" class="auto-style2">
              <tr id="table_001">
                  <td id="table_002" class="auto-style158">
                              <asp:Label ID="mov_ch_text" runat="server" Text="空白"></asp:Label>
                              </td>
                  <td class="auto-style159">
                              <asp:Label ID="day_text" runat="server" Text="time"></asp:Label>
                              <asp:Label ID="day_int" runat="server" Text="time" Visible="False"></asp:Label>
                              </td>
                  <td class="auto-style160"></td>
              </tr>
              <tr id="table_003">
                  <td class="auto-style155">
                              <asp:Label ID="mov_en_text" runat="server" Text="空白"></asp:Label>
                          </td>
                  <td class="auto-style156">
                              <asp:Label ID="time_text" runat="server" Text="time"></asp:Label>
                              <asp:Label ID="time_int" runat="server" Text="time" Visible="False"></asp:Label>
                          </td>
                  <td class="auto-style157"></td>
              </tr>
              <tr>
                  <td class="auto-style3">訂票編號:<asp:Label ID="mov_id_text" runat="server" Text="空白"></asp:Label>
                          </td>
                  <td class="auto-style4">
                              <asp:Label ID="theater_text" runat="server" Text="theater"></asp:Label>
                          </td>
                  <td>&nbsp;</td>
              </tr>
          </table>
      </div>
      <div>
          <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
          <div id="center2" class="auto-style6">

            <asp:Image ID="Image1" runat="server" ImageUrl="~/images/seats/screen.gif" ImageAlign="AbsMiddle" />
        </div>
                  </div>
                  <asp:UpdatePanel ID="UpdatePanel1" runat="server">
                      <ContentTemplate>
                          <table class="auto-style12" id="my_table">
                              <tr>
                                  <td class="auto-style199">&nbsp;&nbsp;</td>
                                  <td class="auto-style13"></td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style16">&nbsp;</td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style18">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style40">&nbsp;</td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td class="auto-style177">&nbsp;</td>
                                  <td class="auto-style188">&nbsp;</td>
                              </tr>
                              <tr>
                                  <td class="auto-style77">
                                      A</td>
                                  <td class="auto-style78">
                                      <asp:Button ID="A1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="A1_Click" />
                                  </td>
                                  <td class="auto-style79">
                                      <asp:Button ID="A2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="A2_Click" />
                                  </td>
                                  <td class="auto-style80">
                                      <asp:Button ID="A3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="A3_Click" />
                                  </td>
                                  <td class="auto-style80">
                                      <asp:Button ID="A4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="A4_Click" />
                                  </td>
                                  <td class="auto-style80">
                                      <asp:Button ID="A5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="A5_Click" />
                                  </td>
                                  <td class="auto-style78">
                                      <asp:Button ID="A6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="A6_Click" />
                                  </td>
                                  <td class="auto-style80">
                                      <asp:Button ID="A7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="A7_Click" />
                                  </td>
                                  <td class="auto-style79"></td>
                                  <td class="auto-style79">
                                      <asp:Button ID="A8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="A8_Click"/>
                                  </td>
                                  <td class="auto-style80">
                                      <asp:Button ID="A9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="A9_Click" />
                                  </td>
                                  <td class="auto-style81">
                                      <asp:Button ID="A10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="A10_Click" />
                                  </td>
                                  <td class="auto-style82">
                                      <asp:Button ID="A11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="A11_Click"/>
                                  </td>
                                  <td class="auto-style79">
                                      <asp:Button ID="A12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="A12_Click"/>
                                  </td>
                                  <td class="auto-style78">
                                      <asp:Button ID="A13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="A13_Click"/>
                                  </td>
                                  <td class="auto-style78">
                                      <asp:Button ID="A14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="A14_Click" />
                                  </td>
                                  <td class="auto-style83">
                                      <asp:Button ID="A15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="A15_Click"/>
                                  </td>
                                  <td class="auto-style78">
                                      <asp:Button ID="A16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="A16_Click"/>
                                  </td>
                                  <td class="auto-style84">
                                      <asp:Button ID="A17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="A17_Click"/>
                                  </td>
                                  <td class="auto-style82"></td>
                                  <td class="auto-style83">
                                      <asp:Button ID="A18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="A18_Click"/>
                                  </td>
                                  <td class="auto-style85">
                                      <asp:Button ID="A19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="A19_Click"/>
                                  </td>
                                  <td class="auto-style85">
                                      <asp:Button ID="A20" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="A20_Click"/>
                                  </td>
                                  <td class="auto-style85">
                                      <asp:Button ID="A21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="A21_Click"/>
                                  </td>
                                  <td class="auto-style85">
                                      <asp:Button ID="A22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="A22_Click"/>
                                  </td>
                                  <td class="auto-style85">
                                      <asp:Button ID="A23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="A23_Click"/>
                                  </td>
                                  <td class="auto-style178">
                                      <asp:Button ID="A24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="A24_Click" />
                                  </td>
                                  <td class="auto-style189">
                                      <asp:Label ID="Label2" runat="server" Text="A"></asp:Label>
                                  </td>
                              </tr>
                              <tr>
                                  <td class="auto-style67">B</td>
                                  <td class="auto-style68">
                                      <asp:Button ID="B1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="B1_Click"/>
                                  </td>
                                  <td class="auto-style69">
                                      <asp:Button ID="B2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="B2_Click"/>
                                  </td>
                                  <td class="auto-style70">
                                      <asp:Button ID="B3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="B3_Click"/>
                                  </td>
                                  <td class="auto-style70">
                                      <asp:Button ID="B4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="B4_Click"/>
                                  </td>
                                  <td class="auto-style70">
                                      <asp:Button ID="B5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="B5_Click"/>
                                  </td>
                                  <td class="auto-style68">
                                      <asp:Button ID="B6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="B6_Click"/>
                                  </td>
                                  <td class="auto-style70">
                                      <asp:Button ID="B7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="B7_Click"/>
                                  </td>
                                  <td class="auto-style69"></td>
                                  <td class="auto-style69">
                                      <asp:Button ID="B8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="B8_Click"/>
                                  </td>
                                  <td class="auto-style70">
                                      <asp:Button ID="B9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="B9_Click"/>
                                  </td>
                                  <td class="auto-style71">
                                      <asp:Button ID="B10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="B10_Click"/>
                                  </td>
                                  <td class="auto-style72">
                                      <asp:Button ID="B11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="B11_Click"/>
                                  </td>
                                  <td class="auto-style69">
                                      <asp:Button ID="B12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="B12_Click"/>
                                  </td>
                                  <td class="auto-style68">
                                      <asp:Button ID="B13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="B13_Click"/>
                                  </td>
                                  <td class="auto-style68">
                                      <asp:Button ID="B14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="B14_Click"/>
                                  </td>
                                  <td class="auto-style73">
                                      <asp:Button ID="B15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="B15_Click"/>
                                  </td>
                                  <td class="auto-style68">
                                      <asp:Button ID="B16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="B16_Click"/>
                                  </td>
                                  <td class="auto-style74">
                                      <asp:Button ID="B17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="B17_Click"/>
                                  </td>
                                  <td class="auto-style72"></td>
                                  <td class="auto-style73">
                                      <asp:Button ID="B18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="B18_Click"/>
                                  </td>
                                  <td class="auto-style75">
                                      <asp:Button ID="B19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="B19_Click"/>
                                  </td>
                                  <td class="auto-style75">
                                      <asp:Button ID="B20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="B20_Click"/>
                                  </td>
                                  <td class="auto-style75">
                                      <asp:Button ID="B21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="B21_Click"/>
                                  </td>
                                  <td class="auto-style75">
                                      <asp:Button ID="B22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="B22_Click"/>
                                  </td>
                                  <td class="auto-style75">
                                      <asp:Button ID="B23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="B23_Click"/>
                                  </td>
                                  <td class="auto-style179">
                                      <asp:Button ID="B24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="B24_Click"/>
                                  </td>
                                  <td class="auto-style190">B</td>
                              </tr>
                              <tr>
                                  <td class="auto-style57">C</td>
                                  <td class="auto-style58">
                                      <asp:Button ID="C1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="C1_Click"/>
                                  </td>
                                  <td class="auto-style59">
                                      <asp:Button ID="C2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="C2_Click" />
                                  </td>
                                  <td class="auto-style60">
                                      <asp:Button ID="C3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="C3_Click"/>
                                  </td>
                                  <td class="auto-style60">
                                      <asp:Button ID="C4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="C4_Click"/>
                                  </td>
                                  <td class="auto-style60">
                                      <asp:Button ID="C5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="C5_Click"/>
                                  </td>
                                  <td class="auto-style58">
                                      <asp:Button ID="C6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="C6_Click"/>
                                  </td>
                                  <td class="auto-style60">
                                      <asp:Button ID="C7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="C7_Click"/>
                                  </td>
                                  <td class="auto-style59"></td>
                                  <td class="auto-style59">
                                      <asp:Button ID="C8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="C8_Click"/>
                                  </td>
                                  <td class="auto-style60">
                                      <asp:Button ID="C9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="C9_Click"/>
                                  </td>
                                  <td class="auto-style61">
                                      <asp:Button ID="C10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="C10_Click"/>
                                  </td>
                                  <td class="auto-style62">
                                      <asp:Button ID="C11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="C11_Click"/>
                                  </td>
                                  <td class="auto-style59">
                                      <asp:Button ID="C12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="C12_Click"/>
                                  </td>
                                  <td class="auto-style58">
                                      <asp:Button ID="C13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="C13_Click"/>
                                  </td>
                                  <td class="auto-style58">
                                      <asp:Button ID="C14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="C14_Click"/>
                                  </td>
                                  <td class="auto-style63">
                                      <asp:Button ID="C15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="C15_Click"/>
                                  </td>
                                  <td class="auto-style58">
                                      <asp:Button ID="C16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="C16_Click"/>
                                  </td>
                                  <td class="auto-style64">
                                      <asp:Button ID="C17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="C17_Click"/>
                                  </td>
                                  <td class="auto-style62"></td>
                                  <td class="auto-style63">
                                      <asp:Button ID="C18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="C18_Click"/>
                                  </td>
                                  <td class="auto-style65">
                                      <asp:Button ID="C19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="C19_Click"/>
                                  </td>
                                  <td class="auto-style65">
                                      <asp:Button ID="C20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="C20_Click"/>
                                  </td>
                                  <td class="auto-style65">
                                      <asp:Button ID="C21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="C21_Click"/>
                                  </td>
                                  <td class="auto-style65">
                                      <asp:Button ID="C22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="C22_Click"/>
                                  </td>
                                  <td class="auto-style65">
                                      <asp:Button ID="C23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="C23_Click"/>
                                  </td>
                                  <td class="auto-style180">
                                      <asp:Button ID="C24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="C24_Click"/>
                                  </td>
                                  <td class="auto-style191">C</td>
                              </tr>
                              <tr>
                                  <td class="auto-style87">D&nbsp;</td>
                                  <td class="auto-style88">
                                      <asp:Button ID="D1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="D1_Click"/>
                                  </td>
                                  <td class="auto-style89">
                                      <asp:Button ID="D2" runat="server" BackColor="Silver" Height="30px"  Text="2" Width="30px" OnClick="D2_Click"/>
                                  </td>
                                  <td class="auto-style90">
                                      <asp:Button ID="D3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="D3_Click"/>
                                  </td>
                                  <td class="auto-style90">
                                      <asp:Button ID="D4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="D4_Click"/>
                                  </td>
                                  <td class="auto-style90">
                                      <asp:Button ID="D5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="D5_Click"/>
                                  </td>
                                  <td class="auto-style88">
                                      <asp:Button ID="D6" runat="server" BackColor="Silver" Height="30px"  Text="6" Width="30px" OnClick="D6_Click"/>
                                  </td>
                                  <td class="auto-style90">
                                      <asp:Button ID="D7" runat="server" BackColor="Silver" Height="30px"  Text="7" Width="30px" OnClick="D7_Click"/>
                                  </td>
                                  <td class="auto-style89"></td>
                                  <td class="auto-style89">
                                      <asp:Button ID="D8" runat="server" BackColor="Silver" Height="30px"  Text="8" Width="30px" OnClick="D8_Click"/>
                                  </td>
                                  <td class="auto-style90">
                                      <asp:Button ID="D9" runat="server" BackColor="Silver" Height="30px"  Text="9" Width="30px" OnClick="D9_Click"/>
                                  </td>
                                  <td class="auto-style91">
                                      <asp:Button ID="D10" runat="server" BackColor="Silver" Height="30px"  Text="10" Width="30px" OnClick="D10_Click"/>
                                  </td>
                                  <td class="auto-style92">
                                      <asp:Button ID="D11" runat="server" BackColor="Silver" Height="30px"  Text="11" Width="30px" OnClick="D11_Click"/>
                                  </td>
                                  <td class="auto-style89">
                                      <asp:Button ID="D12" runat="server" BackColor="Silver" Height="30px"  Text="12" Width="30px" OnClick="D12_Click"/>
                                  </td>
                                  <td class="auto-style88">
                                      <asp:Button ID="D13" runat="server" BackColor="Silver" Height="30px"  Text="13" Width="30px" OnClick="D13_Click"/>
                                  </td>
                                  <td class="auto-style88">
                                      <asp:Button ID="D14" runat="server" BackColor="Silver" Height="30px"  Text="14" Width="30px" OnClick="D14_Click"/>
                                  </td>
                                  <td class="auto-style93">
                                      <asp:Button ID="D15" runat="server" BackColor="Silver" Height="30px"  Text="15" Width="30px" OnClick="D15_Click"/>
                                  </td>
                                  <td class="auto-style88">
                                      <asp:Button ID="D16" runat="server" BackColor="Silver" Height="30px"  Text="16" Width="30px" OnClick="D16_Click"/>
                                  </td>
                                  <td class="auto-style94">
                                      <asp:Button ID="D17" runat="server" BackColor="Silver" Height="30px"  Text="17" Width="30px" OnClick="D17_Click"/>
                                  </td>
                                  <td class="auto-style92"></td>
                                  <td class="auto-style93">
                                      <asp:Button ID="D18" runat="server" BackColor="Silver" Height="30px"  Text="18" Width="30px" OnClick="D18_Click"/>
                                  </td>
                                  <td class="auto-style95">
                                      <asp:Button ID="D19" runat="server" BackColor="Silver" Height="30px"  Text="19" Width="30px" OnClick="D19_Click"/>
                                  </td>
                                  <td class="auto-style95">
                                      <asp:Button ID="D20" runat="server" BackColor="Silver" Height="30px"  Text="20" Width="30px" OnClick="D20_Click"/>
                                  </td>
                                  <td class="auto-style95">
                                      <asp:Button ID="D21" runat="server" BackColor="Silver" Height="30px"  Text="21" Width="30px" OnClick="D21_Click"/>
                                  </td>
                                  <td class="auto-style95">
                                      <asp:Button ID="D22" runat="server" BackColor="Silver" Height="30px"  Text="22" Width="30px" OnClick="D22_Click"/>
                                  </td>
                                  <td class="auto-style95">
                                      <asp:Button ID="D23" runat="server" BackColor="Silver" Height="30px"  Text="23" Width="30px" OnClick="D23_Click"/>
                                  </td>
                                  <td class="auto-style181">
                                      <asp:Button ID="D24" runat="server" BackColor="Silver" Height="30px"  Text="24" Width="30px" OnClick="D24_Click"/>
                                  </td>
                                  <td class="auto-style192">D</td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">E</td>
                                  <td class="auto-style13">
                                      <asp:Button ID="E1" runat="server" BackColor="Silver" Height="30px"  Text="1" Width="30px" OnClick="E1_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="E2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="E2_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="E3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="E3_Click" />
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="E4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="E4_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="E5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="E5_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="E6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="E6_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="E7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="E7_Click"/>
                                  </td>
                                  <td class="auto-style14"></td>
                                  <td class="auto-style14">
                                      <asp:Button ID="E8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="E8_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="E9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="E9_Click"/>
                                  </td>
                                  <td class="auto-style16">
                                      <asp:Button ID="E10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="E10_Click"/>
                                  </td>
                                  <td class="auto-style17">
                                      <asp:Button ID="E11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="E11_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="E12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="E12_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="E13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="E13_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="E14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="E14_Click"/>
                                  </td>
                                  <td class="auto-style18">
                                      <asp:Button ID="E15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="E15_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="E16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="E16_Click"/>
                                  </td>
                                  <td class="auto-style40">
                                      <asp:Button ID="E17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="E17_Click"/>
                                  </td>
                                  <td class="auto-style17"></td>
                                  <td class="auto-style18">
                                      <asp:Button ID="E18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="E18_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="E19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="E19_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="E20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="E20_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="E21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="E21_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="E22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="E22_Click" />
                                  </td>
                                  <td>
                                      <asp:Button ID="E23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="E23_Click"/>
                                  </td>
                                  <td class="auto-style177">
                                      <asp:Button ID="E24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="E24_Click"/>
                                  </td>
                                  <td class="auto-style188">E</td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">&nbsp;&nbsp;</td>
                                  <td class="auto-style13"></td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style16">&nbsp;</td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style18">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style40">&nbsp;</td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td class="auto-style177">&nbsp;</td>
                                  <td class="auto-style188">&nbsp;</td>
                              </tr>
                              <tr>
                                  <td class="auto-style44">F</td>
                                  <td class="auto-style21">
                                      <asp:Button ID="F1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="F1_Click"/>
                                  </td>
                                  <td class="auto-style22">
                                      <asp:Button ID="F2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="F2_Click"/>
                                  </td>
                                  <td class="auto-style23">
                                      <asp:Button ID="F3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="F3_Click"/>
                                  </td>
                                  <td class="auto-style23">
                                      <asp:Button ID="F4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="F4_Click"/>
                                  </td>
                                  <td class="auto-style23">
                                      <asp:Button ID="F5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="F5_Click"/>
                                  </td>
                                  <td class="auto-style21">
                                      <asp:Button ID="F6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="F6_Click"/>
                                  </td>
                                  <td class="auto-style23">
                                      <asp:Button ID="F7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="F7_Click"/>
                                  </td>
                                  <td class="auto-style22"></td>
                                  <td class="auto-style22">
                                      <asp:Button ID="F8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="F8_Click"/>
                                  </td>
                                  <td class="auto-style23">
                                      <asp:Button ID="F9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="F9_Click"/>
                                  </td>
                                  <td class="auto-style24">
                                      <asp:Button ID="F10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="F10_Click"/>
                                  </td>
                                  <td class="auto-style25">
                                      <asp:Button ID="F11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="F11_Click"/>
                                  </td>
                                  <td class="auto-style22">
                                      <asp:Button ID="F12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="F12_Click"/>
                                  </td>
                                  <td class="auto-style21">
                                      <asp:Button ID="F13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="F13_Click"/>
                                  </td>
                                  <td class="auto-style21">
                                      <asp:Button ID="F14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="F14_Click"/>
                                  </td>
                                  <td class="auto-style26">
                                      <asp:Button ID="F15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="F15_Click"/>
                                  </td>
                                  <td class="auto-style21">
                                      <asp:Button ID="F16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="F16_Click"/>
                                  </td>
                                  <td class="auto-style41">
                                      <asp:Button ID="F17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="F17_Click"/>
                                  </td>
                                  <td class="auto-style25"></td>
                                  <td class="auto-style26">
                                      <asp:Button ID="F18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="F18_Click"/>
                                  </td>
                                  <td class="auto-style20">
                                      <asp:Button ID="F19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="F19_Click"/>
                                  </td>
                                  <td class="auto-style20">
                                      <asp:Button ID="F20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="F20_Click"/>
                                  </td>
                                  <td class="auto-style20">
                                      <asp:Button ID="F21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="F21_Click"/>
                                  </td>
                                  <td class="auto-style20">
                                      <asp:Button ID="F22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="F22_Click"/>
                                  </td>
                                  <td class="auto-style20">
                                      <asp:Button ID="F23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="F23_Click"/>
                                  </td>
                                  <td class="auto-style182">
                                      <asp:Button ID="F24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="F24_Click"/>
                                  </td>
                                  <td class="auto-style193">F</td>
                              </tr>
                              <tr>
                                  <td class="auto-style119">G&nbsp;&nbsp;</td>
                                  <td class="auto-style120">
                                      <asp:Button ID="G1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="G1_Click"/>
                                  </td>
                                  <td class="auto-style121">
                                      <asp:Button ID="G2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="G2_Click"/>
                                  </td>
                                  <td class="auto-style122">
                                      <asp:Button ID="G3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="G3_Click"/>
                                  </td>
                                  <td class="auto-style122">
                                      <asp:Button ID="G4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="G4_Click"/>
                                  </td>
                                  <td class="auto-style122">
                                      <asp:Button ID="G5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="G5_Click"/>
                                  </td>
                                  <td class="auto-style120">
                                      <asp:Button ID="G6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="G6_Click"/>
                                  </td>
                                  <td class="auto-style122">
                                      <asp:Button ID="G7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="G7_Click"/>
                                  </td>
                                  <td class="auto-style121"></td>
                                  <td class="auto-style121">
                                      <asp:Button ID="G8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="G8_Click"/>
                                  </td>
                                  <td class="auto-style122">
                                      <asp:Button ID="G9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="G9_Click"/>
                                  </td>
                                  <td class="auto-style123">
                                      <asp:Button ID="G10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="G10_Click"/>
                                  </td>
                                  <td class="auto-style124">
                                      <asp:Button ID="G11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="G11_Click"/>
                                  </td>
                                  <td class="auto-style121">
                                      <asp:Button ID="G12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="G12_Click"/>
                                  </td>
                                  <td class="auto-style120">
                                      <asp:Button ID="G13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="G13_Click"/>
                                  </td>
                                  <td class="auto-style120">
                                      <asp:Button ID="G14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="G14_Click"/>
                                  </td>
                                  <td class="auto-style125">
                                      <asp:Button ID="G15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="G15_Click"/>
                                  </td>
                                  <td class="auto-style120">
                                      <asp:Button ID="G16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="G16_Click"/>
                                  </td>
                                  <td class="auto-style126">
                                      <asp:Button ID="G17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="G17_Click"/>
                                  </td>
                                  <td class="auto-style124"></td>
                                  <td class="auto-style125">
                                      <asp:Button ID="G18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="G18_Click"/>
                                  </td>
                                  <td class="auto-style127">
                                      <asp:Button ID="G19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="G19_Click"/>
                                  </td>
                                  <td class="auto-style127">
                                      <asp:Button ID="G20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="G20_Click"/>
                                  </td>
                                  <td class="auto-style127">
                                      <asp:Button ID="G21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="G21_Click"/>
                                  </td>
                                  <td class="auto-style127">
                                      <asp:Button ID="G22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="G22_Click"/>
                                  </td>
                                  <td class="auto-style127">
                                      <asp:Button ID="G23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="G23_Click"/>
                                  </td>
                                  <td class="auto-style183">
                                      <asp:Button ID="G24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="G24_Click"/>
                                  </td>
                                  <td class="auto-style194">G</td>
                              </tr>
                              <tr>
                                  <td class="auto-style45">H&nbsp;&nbsp;</td>
                                  <td class="auto-style28">
                                      <asp:Button ID="H1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="H1_Click"/>
                                  </td>
                                  <td class="auto-style29">
                                      <asp:Button ID="H2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="H2_Click"/>
                                  </td>
                                  <td class="auto-style30">
                                      <asp:Button ID="H3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="H3_Click"/>
                                  </td>
                                  <td class="auto-style30">
                                      <asp:Button ID="H4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="H4_Click"/>
                                  </td>
                                  <td class="auto-style30">
                                      <asp:Button ID="H5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="H5_Click"/>
                                  </td>
                                  <td class="auto-style28">
                                      <asp:Button ID="H6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="H6_Click"/>
                                  </td>
                                  <td class="auto-style30">
                                      <asp:Button ID="H7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="H7_Click"/>
                                  </td>
                                  <td class="auto-style29"></td>
                                  <td class="auto-style29">
                                      <asp:Button ID="H8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="H8_Click"/>
                                  </td>
                                  <td class="auto-style30">
                                      <asp:Button ID="H9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="H9_Click"/>
                                  </td>
                                  <td class="auto-style31">
                                      <asp:Button ID="H10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="H10_Click"/>
                                  </td>
                                  <td class="auto-style32">
                                      <asp:Button ID="H11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="H11_Click"/>
                                  </td>
                                  <td class="auto-style29">
                                      <asp:Button ID="H12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px"  OnClick="H12_Click"/>
                                  </td>
                                  <td class="auto-style28">
                                      <asp:Button ID="H13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px"  OnClick="H13_Click"/>
                                  </td>
                                  <td class="auto-style28">
                                      <asp:Button ID="H14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px"  OnClick="H14_Click"/>
                                  </td>
                                  <td class="auto-style33">
                                      <asp:Button ID="H15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px"  OnClick="H15_Click"/>
                                  </td>
                                  <td class="auto-style28">
                                      <asp:Button ID="H16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px"  OnClick="H16_Click"/>
                                  </td>
                                  <td class="auto-style42">
                                      <asp:Button ID="H17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px"  OnClick="H17_Click"/>
                                  </td>
                                  <td class="auto-style32"></td>
                                  <td class="auto-style33">
                                      <asp:Button ID="H18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px"  OnClick="H18_Click"/>
                                  </td>
                                  <td class="auto-style27">
                                      <asp:Button ID="H19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px"  OnClick="H19_Click"/>
                                  </td>
                                  <td class="auto-style27">
                                      <asp:Button ID="H20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px"  OnClick="H20_Click"/>
                                  </td>
                                  <td class="auto-style27">
                                      <asp:Button ID="H21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="H21_Click"/>
                                  </td>
                                  <td class="auto-style27">
                                      <asp:Button ID="H22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="H22_Click"/>
                                  </td>
                                  <td class="auto-style27">
                                      <asp:Button ID="H23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="H23_Click"/>
                                  </td>
                                  <td class="auto-style184">
                                      <asp:Button ID="H24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="H24_Click"/>
                                  </td>
                                  <td class="auto-style195">H</td>
                              </tr>
                              <tr>
                                  <td class="auto-style129">I&nbsp;&nbsp;</td>
                                  <td class="auto-style130">
                                      <asp:Button ID="I1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="I1_Click"/>
                                  </td>
                                  <td class="auto-style131">
                                      <asp:Button ID="I2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="I2_Click"/>
                                  </td>
                                  <td class="auto-style132">
                                      <asp:Button ID="I3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="I3_Click"/>
                                  </td>
                                  <td class="auto-style132">
                                      <asp:Button ID="I4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="I4_Click"/>
                                  </td>
                                  <td class="auto-style132">
                                      <asp:Button ID="I5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="I5_Click"/>
                                  </td>
                                  <td class="auto-style130">
                                      <asp:Button ID="I6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="I6_Click"/>
                                  </td>
                                  <td class="auto-style132">
                                      <asp:Button ID="I7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="I7_Click"/>
                                  </td>
                                  <td class="auto-style131"></td>
                                  <td class="auto-style131">
                                      <asp:Button ID="I8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="I8_Click"/>
                                  </td>
                                  <td class="auto-style132">
                                      <asp:Button ID="I9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="I9_Click"/>
                                  </td>
                                  <td class="auto-style133">
                                      <asp:Button ID="I10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="I10_Click"/>
                                  </td>
                                  <td class="auto-style134">
                                      <asp:Button ID="I11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="I11_Click"/>
                                  </td>
                                  <td class="auto-style131">
                                      <asp:Button ID="I12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="I12_Click"/>
                                  </td>
                                  <td class="auto-style130">
                                      <asp:Button ID="I13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="I13_Click"/>
                                  </td>
                                  <td class="auto-style130">
                                      <asp:Button ID="I14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="I14_Click"/>
                                  </td>
                                  <td class="auto-style135">
                                      <asp:Button ID="I15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="I15_Click"/>
                                  </td>
                                  <td class="auto-style130">
                                      <asp:Button ID="I16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="I16_Click"/>
                                  </td>
                                  <td class="auto-style136">
                                      <asp:Button ID="I17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="I17_Click"/>
                                  </td>
                                  <td class="auto-style134"></td>
                                  <td class="auto-style135">
                                      <asp:Button ID="I18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="I18_Click"/>
                                  </td>
                                  <td class="auto-style137">
                                      <asp:Button ID="I19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="I19_Click"/>
                                  </td>
                                  <td class="auto-style137">
                                      <asp:Button ID="I20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="I20_Click"/>
                                  </td>
                                  <td class="auto-style137">
                                      <asp:Button ID="I21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="I21_Click"/>
                                  </td>
                                  <td class="auto-style137">
                                      <asp:Button ID="I22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="I22_Click"/>
                                  </td>
                                  <td class="auto-style137">
                                      <asp:Button ID="I23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="I23_Click"/>
                                  </td>
                                  <td class="auto-style185">
                                      <asp:Button ID="I24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="I24_Click"/>
                                  </td>
                                  <td class="auto-style196">I</td>
                              </tr>
                              <tr>
                                  <td class="auto-style139">J&nbsp;&nbsp;</td>
                                  <td class="auto-style140">
                                      <asp:Button ID="J1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="J1_Click"/>
                                  </td>
                                  <td class="auto-style141">
                                      <asp:Button ID="J2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="J2_Click"/>
                                  </td>
                                  <td class="auto-style142">
                                      <asp:Button ID="J3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="J3_Click"/>
                                  </td>
                                  <td class="auto-style142">
                                      <asp:Button ID="J4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="J4_Click"/>
                                  </td>
                                  <td class="auto-style142">
                                      <asp:Button ID="J5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="J5_Click"/>
                                  </td>
                                  <td class="auto-style140">
                                      <asp:Button ID="J6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="J6_Click"/>
                                  </td>
                                  <td class="auto-style142">
                                      <asp:Button ID="J7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="J7_Click"/>
                                  </td>
                                  <td class="auto-style141"></td>
                                  <td class="auto-style141">
                                      <asp:Button ID="J8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="J8_Click"/>
                                  </td>
                                  <td class="auto-style142">
                                      <asp:Button ID="J9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="J9_Click"/>
                                  </td>
                                  <td class="auto-style143">
                                      <asp:Button ID="J10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="J10_Click"/>
                                  </td>
                                  <td class="auto-style144">
                                      <asp:Button ID="J11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="J11_Click"/>
                                  </td>
                                  <td class="auto-style141">
                                      <asp:Button ID="J12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="J12_Click"/>
                                  </td>
                                  <td class="auto-style140">
                                      <asp:Button ID="J13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="J13_Click"/>
                                  </td>
                                  <td class="auto-style140">
                                      <asp:Button ID="J14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="J14_Click"/>
                                  </td>
                                  <td class="auto-style145">
                                      <asp:Button ID="J15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="J15_Click"/>
                                  </td>
                                  <td class="auto-style140">
                                      <asp:Button ID="J16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="J16_Click"/>
                                  </td>
                                  <td class="auto-style146">
                                      <asp:Button ID="J17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="J17_Click"/>
                                  </td>
                                  <td class="auto-style144"></td>
                                  <td class="auto-style145">
                                      <asp:Button ID="J18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="J18_Click"/>
                                  </td>
                                  <td class="auto-style147">
                                      <asp:Button ID="J19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="J19_Click"/>
                                  </td>
                                  <td class="auto-style147">
                                      <asp:Button ID="J20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="J20_Click"/>
                                  </td>
                                  <td class="auto-style147">
                                      <asp:Button ID="J21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="J21_Click"/>
                                  </td>
                                  <td class="auto-style147">
                                      <asp:Button ID="J22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="J22_Click"/>
                                  </td>
                                  <td class="auto-style147">
                                      <asp:Button ID="J23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="J23_Click"/>
                                  </td>
                                  <td class="auto-style186">
                                      <asp:Button ID="J24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="J24_Click"/>
                                  </td>
                                  <td class="auto-style197">J</td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">&nbsp;</td>
                                  <td class="auto-style13"></td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style16">&nbsp;</td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style18">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style40">&nbsp;</td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td class="auto-style177">&nbsp;</td>
                                  <td class="auto-style188">&nbsp;</td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">K&nbsp;&nbsp;&nbsp;</td>
                                  <td class="auto-style13">
                                      <asp:Button ID="K1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="K1_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="K2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="K2_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="K3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="K3_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="K4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="K4_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="K5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="K5_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="K6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="K6_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="K7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="K7_Click"/>
                                  </td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">
                                      <asp:Button ID="K8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="K8_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="K9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="K9_Click"/>
                                  </td>
                                  <td class="auto-style16">
                                      <asp:Button ID="K10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="K10_Click"/>
                                  </td>
                                  <td class="auto-style17">
                                      <asp:Button ID="K11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="K11_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="K12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="K12_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="K13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="K13_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="K14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="K14_Click"/>
                                  </td>
                                  <td class="auto-style18">
                                      <asp:Button ID="K15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="K15_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="K16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="K16_Click"/>
                                  </td>
                                  <td class="auto-style40">
                                      <asp:Button ID="K17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="K17_Click"/>
                                  </td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">
                                      <asp:Button ID="K18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="K18_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="K19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="K19_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="K20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="K20_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="K21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="K21_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="K22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="K22_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="K23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="K23_Click"/>
                                  </td>
                                  <td class="auto-style177">
                                      <asp:Button ID="K24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="K24_Click"/>
                                  </td>
                                  <td class="auto-style188">K</td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">L&nbsp;&nbsp;</td>
                                  <td class="auto-style13">
                                      <asp:Button ID="L1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="L1_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="L2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="L2_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="L3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="L3_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="L4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="L4_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="L5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="L5_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="L6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="L6_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="L7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="L7_Click"/>
                                  </td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">
                                      <asp:Button ID="L8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="L8_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="L9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="L9_Click"/>
                                  </td>
                                  <td class="auto-style16">
                                      <asp:Button ID="L10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="L10_Click"/>
                                  </td>
                                  <td class="auto-style17">
                                      <asp:Button ID="L11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="L11_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="L12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="L12_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="L13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="L13_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="L14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="L14_Click"/>
                                  </td>
                                  <td class="auto-style18">
                                      <asp:Button ID="L15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="L15_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="L16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="L16_Click"/>
                                  </td>
                                  <td class="auto-style40">
                                      <asp:Button ID="L17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="L17_Click"/>
                                  </td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">
                                      <asp:Button ID="L18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="L18_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="L19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="L19_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="L20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="L20_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="L21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="L21_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="L22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="L22_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="L23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="L23_Click"/>
                                  </td>
                                  <td class="auto-style177">
                                      <asp:Button ID="L24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="L24_Click"/>
                                  </td>
                                  <td class="auto-style188">L</td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">M&nbsp;</td>
                                  <td class="auto-style13">
                                      <asp:Button ID="M1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="M1_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="M2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="M2_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="M3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="M3_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="M4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="M4_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="M5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="M5_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="M6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="M6_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="M7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="M7_Click"/>
                                  </td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">
                                      <asp:Button ID="M8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="M8_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="M9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="M9_Click"/>
                                  </td>
                                  <td class="auto-style16">
                                      <asp:Button ID="M10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="M10_Click"/>
                                  </td>
                                  <td class="auto-style17">
                                      <asp:Button ID="M11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="M11_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="M12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="M12_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="M13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="M13_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="M14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="M14_Click"/>
                                  </td>
                                  <td class="auto-style18">
                                      <asp:Button ID="M15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="M15_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="M16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="M16_Click"/>
                                  </td>
                                  <td class="auto-style40">
                                      <asp:Button ID="M17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="M17_Click"/>
                                  </td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">
                                      <asp:Button ID="M18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="M18_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="M19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="M19_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="M20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="M20_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="M21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="M21_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="M22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="M22_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="M23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="M23_Click"/>
                                  </td>
                                  <td class="auto-style177">
                                      <asp:Button ID="M24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="M24_Click"/>
                                  </td>
                                  <td class="auto-style188">M</td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">N&nbsp;&nbsp;</td>
                                  <td class="auto-style13">
                                      <asp:Button ID="N1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="N1_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="N2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="N2_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="N3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="N3_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="N4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="N4_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="N5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="N5_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="N6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="N6_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="N7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="N7_Click"/>
                                  </td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">
                                      <asp:Button ID="N8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="N8_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="N9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="N9_Click"/>
                                  </td>
                                  <td class="auto-style16">
                                      <asp:Button ID="N10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="N10_Click"/>
                                  </td>
                                  <td class="auto-style17">
                                      <asp:Button ID="N11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="N11_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="N12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="N12_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="N13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="N13_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="N14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="N14_Click"/>
                                  </td>
                                  <td class="auto-style18">
                                      <asp:Button ID="N15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="N15_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="N16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="N16_Click"/>
                                  </td>
                                  <td class="auto-style40">
                                      <asp:Button ID="N17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="N17_Click"/>
                                  </td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">
                                      <asp:Button ID="N18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="N18_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="N19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="N19_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="N20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="N20_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="N21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="N21_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="N22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="N22_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="N23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="N23_Click"/>
                                  </td>
                                  <td class="auto-style177">
                                      <asp:Button ID="N24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="N24_Click"/>
                                  </td>
                                  <td class="auto-style188">N</td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">O&nbsp;&nbsp;</td>
                                  <td class="auto-style13">
                                      <asp:Button ID="O1" runat="server" BackColor="Silver" Height="30px" Text="1" Width="30px" OnClick="O1_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="O2" runat="server" BackColor="Silver" Height="30px" Text="2" Width="30px" OnClick="O2_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="O3" runat="server" BackColor="Silver" Height="30px" Text="3" Width="30px" OnClick="O3_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="O4" runat="server" BackColor="Silver" Height="30px" Text="4" Width="30px" OnClick="O4_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="O5" runat="server" BackColor="Silver" Height="30px" Text="5" Width="30px" OnClick="O5_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="O6" runat="server" BackColor="Silver" Height="30px" Text="6" Width="30px" OnClick="O6_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="O7" runat="server" BackColor="Silver" Height="30px" Text="7" Width="30px" OnClick="O7_Click"/>
                                  </td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">
                                      <asp:Button ID="O8" runat="server" BackColor="Silver" Height="30px" Text="8" Width="30px" OnClick="O8_Click"/>
                                  </td>
                                  <td class="auto-style15">
                                      <asp:Button ID="O9" runat="server" BackColor="Silver" Height="30px" Text="9" Width="30px" OnClick="O9_Click"/>
                                  </td>
                                  <td class="auto-style16">
                                      <asp:Button ID="O10" runat="server" BackColor="Silver" Height="30px" Text="10" Width="30px" OnClick="O10_Click"/>
                                  </td>
                                  <td class="auto-style17">
                                      <asp:Button ID="O11" runat="server" BackColor="Silver" Height="30px" Text="11" Width="30px" OnClick="O11_Click"/>
                                  </td>
                                  <td class="auto-style14">
                                      <asp:Button ID="O12" runat="server" BackColor="Silver" Height="30px" Text="12" Width="30px" OnClick="O12_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="O13" runat="server" BackColor="Silver" Height="30px" Text="13" Width="30px" OnClick="O13_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="O14" runat="server" BackColor="Silver" Height="30px" Text="14" Width="30px" OnClick="O14_Click"/>
                                  </td>
                                  <td class="auto-style18">
                                      <asp:Button ID="O15" runat="server" BackColor="Silver" Height="30px" Text="15" Width="30px" OnClick="O15_Click"/>
                                  </td>
                                  <td class="auto-style13">
                                      <asp:Button ID="O16" runat="server" BackColor="Silver" Height="30px" Text="16" Width="30px" OnClick="O16_Click"/>
                                  </td>
                                  <td class="auto-style40">
                                      <asp:Button ID="O17" runat="server" BackColor="Silver" Height="30px" Text="17" Width="30px" OnClick="O17_Click"/>
                                  </td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">
                                      <asp:Button ID="O18" runat="server" BackColor="Silver" Height="30px" Text="18" Width="30px" OnClick="O18_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="O19" runat="server" BackColor="Silver" Height="30px" Text="19" Width="30px" OnClick="O19_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="O20" runat="server" BackColor="Silver" Height="30px" Text="20" Width="30px" OnClick="O20_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="O21" runat="server" BackColor="Silver" Height="30px" Text="21" Width="30px" OnClick="O21_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="O22" runat="server" BackColor="Silver" Height="30px" Text="22" Width="30px" OnClick="O22_Click"/>
                                  </td>
                                  <td>
                                      <asp:Button ID="O23" runat="server" BackColor="Silver" Height="30px" Text="23" Width="30px" OnClick="O23_Click"/>
                                  </td>
                                  <td class="auto-style177">
                                      <asp:Button ID="O24" runat="server" BackColor="Silver" Height="30px" Text="24" Width="30px" OnClick="O24_Click"/>
                                  </td>
                                  <td class="auto-style188">O</td>
                              </tr>
                              <tr>
                                  <td class="auto-style107"></td>
                                  <td class="auto-style149">
                                  </td>
                                  <td class="auto-style150"></td>
                                  <td class="auto-style151"></td>
                                  <td class="auto-style151"></td>
                                  <td class="auto-style151"></td>
                                  <td class="auto-style149"></td>
                                  <td class="auto-style151"></td>
                                  <td class="auto-style150"></td>
                                  <td class="auto-style150"></td>
                                  <td class="auto-style151"></td>
                                  <td class="auto-style111"></td>
                                  <td class="auto-style152"></td>
                                  <td class="auto-style150"></td>
                                  <td class="auto-style149"></td>
                                  <td class="auto-style149"></td>
                                  <td class="auto-style153"></td>
                                  <td class="auto-style149"></td>
                                  <td class="auto-style114"></td>
                                  <td class="auto-style152"></td>
                                  <td class="auto-style153"></td>
                                  <td class="auto-style154"></td>
                                  <td class="auto-style154"></td>
                                  <td class="auto-style154"></td>
                                  <td class="auto-style154"></td>
                                  <td class="auto-style154"></td>
                                  <td class="auto-style187"></td>
                                  <td class="auto-style198"></td>
                              </tr>
                              <tr>
                                  <td class="auto-style199">&nbsp;&nbsp;</td>
                                  <td class="auto-style13"></td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style15">&nbsp;</td>
                                  <td class="auto-style16">&nbsp;</td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style14">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style18">&nbsp;</td>
                                  <td class="auto-style13">&nbsp;</td>
                                  <td class="auto-style40">&nbsp;</td>
                                  <td class="auto-style17">&nbsp;</td>
                                  <td class="auto-style18">&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td class="auto-style177">&nbsp;</td>
                                  <td class="auto-style188">&nbsp;</td>
                              </tr>
                          </table>
                          <div>
                              <asp:UpdatePanel ID="UpdatePanel2" runat="server">
                                  <ContentTemplate>
                                      剩餘座位數 :
                                      <asp:Label ID="total_ticket" runat="server" Text="360"></asp:Label>
                                      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 您購買的座位數:&nbsp;
                                      <asp:Label ID="customer_ticket" runat="server"></asp:Label>
                                      &nbsp;&nbsp; 藍色:已預訂&nbsp; 灰色:未預訂&nbsp; 紅色:您當前選定&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;系統資訊:&nbsp;
                                      <asp:Label ID="masg_text" runat="server"></asp:Label>
                                  </ContentTemplate>
                              </asp:UpdatePanel>
                              <div class="auto-style174">
                              </div>
                          </div>
                      </ContentTemplate>
                  </asp:UpdatePanel>
      <div class="auto-style201">
                      <asp:Button ID="ticker_price_back" runat="server" CssClass="auto-style175" Height="38px" Text="回上一步" Width="159px" OnClick="ticker_price_back_Click" />
                      <asp:Button ID="ticker_price_next" runat="server" CssClass="auto-style176" Height="38px" Text="下一步" Width="159px" OnClick="ticker_price_next_Click" />
      </div>
    <p>
        &nbsp;</p>
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
    <p>
        &nbsp;</p>
</body>
</html>
