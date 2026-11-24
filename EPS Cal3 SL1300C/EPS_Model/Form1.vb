Imports System
Imports System.IO.Ports
Imports System.Net.Sockets
Imports System.Text
Imports System.Data
Imports System.Data.OleDb
Imports System.Data.SqlClient
Imports System.IO
Imports Microsoft.Win32
Imports System.Data.SqlClient.SqlException
Imports System.Net
Imports System.Threading

Public Class frmModel
    Dim startTime As DateTime
    Dim endTime As DateTime
    Dim SourceType As Byte
    Dim NoOfMUT As Byte
    Dim CalStep As Byte
    Dim RefMeterFail As Byte
    Dim RefMeterVoltFail As Byte
    Dim RefMeterAmpFail As Byte
    Dim MeterReadFail(6) As Byte
    Dim MeterSACardNo(6) As Long
    Dim MeterStatus(6) As Byte
    Dim PollType As Byte
    Dim Calno As Long
    Dim AccClass(6) As Byte
    Dim InitCount(6) As Byte
    Dim CCWrite(6) As Byte
    Dim CalConstantStr(6) As String
    Dim DCCProcess(6) As Byte
    Dim VerfStep As Byte
    Dim CalConstData(6, 43)
    Dim ProcessType As Byte
    Dim VerVolt(7) As Single
    Dim VerAmps(7) As Single
    Dim VerPF(7) As Single
    Dim VerPFDeg(7) As Single
    Dim Sample(18) As Byte
    Dim SNG_Step As Byte
    'Dim MeterRxBytes(18) As Byte
    Dim MeterRxBytes(6) As Integer
    Dim rxBuff(6) As String
    Dim MeterSlNo As Long
    Dim CalMeterData(6, 29, 200) As Single
    Dim VerMeterData(6, 29, 200) As Single
    Dim AveVerDataSteps(6, 29, 7) As Single
    Dim VerErrData(6, 29, 7) As Single
    Dim MeterData(6, 7, 29) As Single
    Dim AveCalData(6, 7, 29) As Single
    Dim CalVolt(5) As Single
    Dim CalAmps(5) As Single
    Dim CalPF(5) As Single
    Dim CalPFDeg(5) As Single
    Dim MeterIP(24) As String
    Dim RefMeterIP As String = ""
    Dim SourceIP As String = ""
    Dim ET600ReadStep(6) As Byte
    Dim ET600ReadWaiting(6) As Boolean
    Dim ET600ReadTimeout(6) As Byte
    Dim DispatchSlNo(6) As UInteger
    Dim ET600RefCurrent(5) As Single

    ' Voltage R / Y / B
    Dim ET600VoltageCal(6, 3) As Short

    ' 5 levels x 12 available channel locations
    ' ET600 uses only CH1-CH6
    Dim ET600CurrentCal(6, 5, 12) As Short
    Dim ET600WattCal(6, 5, 12) As UShort
    Dim ET600VarCal(6, 5, 12) As Short

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        'Dim TString As String
        'TString = dec_to_float(18)

        ComboBox1.SelectedIndex = 0
        ComboBox2.SelectedIndex = 0

        ComboBox3.IntegralHeight = False
        ComboBox3.DropDownHeight = 500
        ComboBox3.MaxDropDownItems = 50

        'SourceType = 2

        cmbSourceType.Items.Clear()
        cmbSourceType.Items.Add("SNG")
        cmbSourceType.Items.Add("APC")

        ' Default source is APC
        cmbSourceType.SelectedIndex = 1

        SourceType = 2
        NoOfMUT = 1

        Calno = Now.ToString("ddMMyyHHmm")
        GirdAlign()

        CalVolt(1) = 240
        CalAmps(1) = 5  '5
        CalPF(1) = -0.5
        CalPFDeg(1) = 60

        CalVolt(2) = 240
        CalAmps(2) = 2.5 '2.5
        CalPF(2) = -0.5
        CalPFDeg(2) = 60

        CalVolt(3) = 240
        CalAmps(3) = 1  '1
        CalPF(3) = -0.5
        CalPFDeg(3) = 60

        CalVolt(4) = 240
        CalAmps(4) = 0.5    '0.5
        CalPF(4) = -0.5
        CalPFDeg(4) = 60

        CalVolt(5) = 240
        CalAmps(5) = 0.25    '0.25
        CalPF(5) = -0.5
        CalPFDeg(5) = 60

        VerVolt(1) = 240
        VerAmps(1) = 5   '5
        VerPF(1) = 1
        VerPFDeg(1) = 0

        VerVolt(2) = 240
        VerAmps(2) = 5  '5
        VerPF(2) = 0.8
        VerPFDeg(2) = 323.4

        VerVolt(3) = 240
        VerAmps(3) = 5  '5
        VerPF(3) = -0.5
        VerPFDeg(3) = 60

        VerVolt(4) = 240
        VerAmps(4) = 1  '1
        VerPF(4) = 1
        VerPFDeg(4) = 0

        VerVolt(5) = 240
        VerAmps(5) = 1    '1
        VerPF(5) = 0.8
        VerPFDeg(5) = 323.4

        VerVolt(6) = 240
        VerAmps(6) = 1   '1
        VerPF(6) = -0.5
        VerPFDeg(6) = 60

        VerVolt(7) = 200
        VerAmps(7) = 1  '1
        VerPF(7) = -0.5
        VerPFDeg(7) = 60

        'SourceAxWinsock1.Close()
        'SourceAxWinsock1.RemoteHost = "172.16.16.110"
        'SourceAxWinsock1.RemotePort = 4001
        'SourceAxWinsock1.Connect()
        'Thread.Sleep(2000)

        'SocketConnect(0)
        'Thread.Sleep(200)

        If NoOfMUT >= 1 Then
            SocketConnect(1)
            Thread.Sleep(200)
        End If
        If NoOfMUT >= 2 Then
            SocketConnect(2)
            Thread.Sleep(200)
        End If
        If NoOfMUT >= 3 Then
            SocketConnect(3)
            Thread.Sleep(200)
        End If
        If NoOfMUT >= 4 Then
            SocketConnect(4)
            Thread.Sleep(200)
        End If
        If NoOfMUT >= 5 Then
            SocketConnect(5)
            Thread.Sleep(200)
        End If
        If NoOfMUT >= 6 Then
            SocketConnect(6)
            Thread.Sleep(200)
        End If

    End Sub

    Private Sub ConnectSrc()

        Try

            If String.IsNullOrWhiteSpace(SourceIP) Then
                MsgBox("Source IP not loaded from database.")
                Exit Sub
            End If

            SourceAxWinsock1.Close()
            SourceAxWinsock1.RemoteHost = SourceIP
            SourceAxWinsock1.RemotePort = 4001
            SourceAxWinsock1.Connect()

            Thread.Sleep(2000)

            SocketConnect(0)
            Thread.Sleep(200)

        Catch ex As Exception
            MsgBox("ConnectSrc : " & ex.Message)
        End Try

    End Sub

    Private Sub ConnectServer()

        Dim strConnection As String = ""
        Dim ELNetCon As SqlConnection = New SqlConnection(strConnection)

        Try
            strConnection = "Data Source=192.168.5.200; Initial Catalog=ELmeasure;Integrated Security=False;User ID=sa; Password=mEA9269mK*$47@bAG"
            ELNetCon = New SqlConnection(strConnection)
            ELNetCon.Open()

        Catch ex As Exception
            If ELNetCon.State = ConnectionState.Closed Then
                MsgBox("Unable to conecct Elmeasure Server!", vbCritical)
                ErrorEvents("ConnectServer : Unable to conecct Elmeasure Server! Cal No=" + Str(Calno))
                End
            End If
        End Try

    End Sub

    Private Sub cmbSourceType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbSourceType.SelectedIndexChanged

        Select Case cmbSourceType.SelectedIndex
            Case 0
                SourceType = 1   ' SNG
            Case 1
                SourceType = 2   ' APC
        End Select

    End Sub

    Private Sub SocketConnect(ByVal MetetNo As Byte)

        Try

            If MetetNo = 0 Then
                AxWinsock0.Close()
                AxWinsock0.RemoteHost = RefMeterIP
                AxWinsock0.RemotePort = 4001
                AxWinsock0.Connect()
            End If

            If MetetNo = 1 Then
                AxWinsock1.Close()
                AxWinsock1.RemoteHost = MeterIP(1)
                AxWinsock1.RemotePort = 4001
                AxWinsock1.Connect()
            End If

            If MetetNo = 2 Then
                AxWinsock2.Close()
                AxWinsock2.RemoteHost = MeterIP(2)
                AxWinsock2.RemotePort = 4001
                AxWinsock2.Connect()
            End If

            If MetetNo = 3 Then
                AxWinsock3.Close()
                AxWinsock3.RemoteHost = MeterIP(3)
                AxWinsock3.RemotePort = 4001
                AxWinsock3.Connect()
            End If

            If MetetNo = 4 Then
                AxWinsock4.Close()
                AxWinsock4.RemoteHost = MeterIP(4)
                AxWinsock4.RemotePort = 4001
                AxWinsock4.Connect()
            End If

            If MetetNo = 5 Then
                AxWinsock5.Close()
                AxWinsock5.RemoteHost = MeterIP(5)
                AxWinsock5.RemotePort = 4001
                AxWinsock5.Connect()
            End If

            If MetetNo = 6 Then
                AxWinsock6.Close()
                AxWinsock6.RemoteHost = MeterIP(6)
                AxWinsock6.RemotePort = 4001
                AxWinsock6.Connect()
            End If

        Catch ex As Exception
            MsgBox("SocketConnect : " + ex.Message)
        End Try

    End Sub

    Private Sub GirdAlign()

        DataGridView1.EnableHeadersVisualStyles = False
        DataGridView1.Rows.Clear()
        DataGridView1.Columns.Clear()

        Dim ColHead As New DataGridViewTextBoxColumn

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "Sl.No" : ColHead.HeaderText = "Sl.No"
        DataGridView1.Columns.Add(ColHead)

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "Volts" : ColHead.HeaderText = "Volts"
        DataGridView1.Columns.Add(ColHead)

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "Amps" : ColHead.HeaderText = "Amps"
        DataGridView1.Columns.Add(ColHead)

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "PF" : ColHead.HeaderText = "PF"
        DataGridView1.Columns.Add(ColHead)


        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "PFR" : ColHead.HeaderText = "PF R"
        DataGridView1.Columns.Add(ColHead)
        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "PFY" : ColHead.HeaderText = "PF Y"
        DataGridView1.Columns.Add(ColHead)

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "PFB" : ColHead.HeaderText = "PF B"
        DataGridView1.Columns.Add(ColHead)

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "Watts" : ColHead.HeaderText = "Watts-T"
        DataGridView1.Columns.Add(ColHead)

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "Watts" : ColHead.HeaderText = "WattsR"
        DataGridView1.Columns.Add(ColHead)

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "Watts" : ColHead.HeaderText = "WattsY"
        DataGridView1.Columns.Add(ColHead)

        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "Watts" : ColHead.HeaderText = "WattsB"
        DataGridView1.Columns.Add(ColHead)


        ColHead = New DataGridViewTextBoxColumn
        ColHead.Name = "Hz." : ColHead.HeaderText = "Hz."
        DataGridView1.Columns.Add(ColHead)

        Dim totalrows As Integer
        Dim a As Integer
        totalrows = 6
        For a = 1 To totalrows + 1
            DataGridView1.Rows.Add(a)
        Next

        'DataGridView1.Columns(0).Width = 70
        For i = 0 To 11
            DataGridView1.Columns(i).Width = 65
        Next

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Timer1.Enabled = True

    End Sub

    Function CRC16(ByVal data() As Byte) As String

        Dim CRC16Lo As Byte, CRC16Hi As Byte 'CRC register
        Dim CL As Byte, CH As Byte 'Polynomial codes & HA001
        Dim SaveHi As Byte, SaveLo As Byte
        Dim i As Integer
        Dim Flag As Integer

        Try
            CRC16Lo = &HFF
            CRC16Hi = &HFF
            CL = &H1
            CH = &HA0

            For i = 0 To UBound(data)
                CRC16Lo = CRC16Lo Xor data(i) 'for each data and CRC register XOR

                For Flag = 0 To 7
                    SaveHi = CRC16Hi
                    SaveLo = CRC16Lo
                    CRC16Hi = CRC16Hi \ 2 'peak shift to the right one
                    CRC16Lo = CRC16Lo \ 2 'shift to the right a low

                    If ((SaveHi And &H1) = &H1) Then 'If the high byte last one for a
                        CRC16Lo = CRC16Lo Or &H80 'then the low byte shifted to the right after the meeting in front of a
                    End If 'Otherwise, auto-fill 0

                    If ((SaveLo And &H1) = &H1) Then 'If the LSB is 1, then XOR with the polynomial codes
                        CRC16Hi = CRC16Hi Xor CH
                        CRC16Lo = CRC16Lo Xor CL
                    End If
                Next Flag
            Next i

            Dim ReturnData(1) As Byte
            ReturnData(0) = CRC16Hi 'CRC high
            ReturnData(1) = CRC16Lo 'CRC low
            If Len(Hex(ReturnData(1))) = 1 Then
                CRC16 = "0" + Hex(ReturnData(1))
            Else
                CRC16 = Hex(ReturnData(1))
            End If
            If Len(Hex(ReturnData(0))) = 1 Then
                CRC16 = CRC16 + "0" + Hex(ReturnData(0))
            Else
                CRC16 = CRC16 + Hex(ReturnData(0))
            End If

        Catch ex As Exception
            MsgBox("CRC16 - " & Err.Description & " - " & Now)
        End Try

    End Function



    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick

        PollMeters(1)

    End Sub

    '=========================================================
    ' ET600 HELPER - CLAMP SIGNED 16 BIT
    '=========================================================
    Private Function ClampInt16(ByVal Value As Double) As Short

        If Double.IsNaN(Value) OrElse Double.IsInfinity(Value) Then
            Return 0
        End If

        If Value > 32767 Then
            Value = 32767
        ElseIf Value < -32768 Then
            Value = -32768
        End If

        Return CShort(Math.Round(Value))

    End Function


    '=========================================================
    ' ET600 HELPER - CLAMP UNSIGNED 16 BIT
    '=========================================================
    Private Function ClampUInt16(ByVal Value As Double) As UShort

        If Double.IsNaN(Value) OrElse Double.IsInfinity(Value) Then
            Return CUShort(32752)      ' F07F default in workbook
        End If

        If Value > 65535 Then
            Value = 65535
        ElseIf Value < 0 Then
            Value = 0
        End If

        Return CUShort(Math.Round(Value))

    End Function


    '=========================================================
    ' SIGNED INT16 -> LITTLE ENDIAN HEX
    '
    ' Example:
    ' 4660 decimal = 1234 hex
    ' result = "3412"
    '=========================================================
    Private Function Int16LEHex(ByVal Value As Short) As String

        Dim B() As Byte = BitConverter.GetBytes(Value)

        Return B(0).ToString("X2") &
           B(1).ToString("X2")

    End Function


    '=========================================================
    ' UNSIGNED INT16 -> LITTLE ENDIAN HEX
    '=========================================================
    Private Function UInt16LEHex(ByVal Value As UShort) As String

        Dim B() As Byte = BitConverter.GetBytes(Value)

        Return B(0).ToString("X2") &
           B(1).ToString("X2")

    End Function


    '=========================================================
    ' FLOAT -> LITTLE ENDIAN HEX (DCBA)
    '
    ' Workbook requires reference current as:
    ' Float Little Endian (DCBA)
    '
    ' 5.0 -> 00 00 A0 40
    '=========================================================
    Private Function FloatLEHex(ByVal Value As Single) As String

        Dim B() As Byte = BitConverter.GetBytes(Value)

        Return B(0).ToString("X2") &
           B(1).ToString("X2") &
           B(2).ToString("X2") &
           B(3).ToString("X2")

    End Function


    '=========================================================
    ' PF SAFETY
    ' Math.Acos only accepts -1 through +1
    '=========================================================
    Private Function ClampPF(ByVal PF As Double) As Double

        If PF > 1 Then PF = 1
        If PF < -1 Then PF = -1

        Return PF

    End Function



    Private Sub PollMeters(ByVal MeterNo)

        'Try
        Dim Data_send As String
        Dim j As Integer
        Dim Data(5) As Byte
        Dim SendSring As String
        Dim CRC As String
        Dim WS_State As Short
        Dim comBuffer As Byte()
        Dim TempVolt As Single
        Dim TempAmps As Single
        Dim TempData As String

        'If MeterNo = 0 And PollType = 6 Or PollType = 4 Then
        If MeterNo = 0 AndAlso PollType = 6 Then
            RefMeterFail = RefMeterFail + 1
            If RefMeterFail >= 20 Then
                DisableTimer()
                Label14.Text = "Refernece meter is not Communicating, Stopping the Calibration process !"
                MsgBox("Refernece meter is not Communicating, Stopping the Calibration process !")
                'End
            Else
                Label14.Text = ""
            End If
            If ProcessType = 1 Then
                '     MsgBox("RefV=" & MeterData(0, CalStep, 21) &
                'vbCrLf & "RefI=" & MeterData(0, CalStep, 25) &
                'vbCrLf & "SetV=" & CalVolt(CalStep) &
                'vbCrLf & "SetI=" & CalAmps(CalStep))
                TempVolt = MeterData(0, CalStep, 21) * 0.02
                TempAmps = MeterData(0, CalStep, 25) * 0.04
                If CalVolt(CalStep) > (MeterData(0, CalStep, 21) + TempVolt) Or CalVolt(CalStep) < (MeterData(0, CalStep, 21) - TempVolt) Then
                    RefMeterVoltFail = RefMeterVoltFail + 1
                    Label2.Text = "Reference meter voltage Error ! Comm. Count=" + Str(RefMeterVoltFail)
                    If RefMeterVoltFail > 50 Then
                        DisableTimer()
                        'MsgBox("Reference meter Volt Error ! Reinitialising the Source.")
                        For i = 0 To 6
                            InitCount(i) = 0
                            FillDataGridBlank(i)
                            Sample(i) = 0
                        Next
                        RefMeterVoltFail = 0
                        SNGTimer.Interval = 37000
                        If SourceType = 1 Then
                            SourceAxWinsock1.SendData("A")
                            Thread.Sleep(1000)
                            SourceAxWinsock1.SendData("A")
                            Thread.Sleep(1000)
                            TempData = "v1" + Trim(Str(CalVolt(CalStep))) + " i1" + Trim(Str(CalAmps(CalStep))) + " p1" + Trim(Str(CalPFDeg(CalStep))) + " s"
                            Label4.Text = "Calibration Step1: Volt=" + Str(CalVolt(CalStep)) + " Amps=" + Str(CalAmps(CalStep)) + " PF=" + Str(CalPF(CalStep))
                            ErrorEvents(Label4.Text)
                            SourceAxWinsock1.SendData(TempData)
                        Else
                            Set_APC8000(CalVolt(CalStep), CalAmps(CalStep), CalPF(CalStep))
                        End If
                        Thread.Sleep(35000)
                        EnableTimer()
                        Exit Sub
                    End If
                Else
                    Label14.Text = ""
                    RefMeterVoltFail = 0
                End If
                If CalAmps(CalStep) > (MeterData(0, CalStep, 25) + TempAmps) Or CalAmps(CalStep) < (MeterData(0, CalStep, 25) - TempAmps) Then
                    RefMeterAmpFail = RefMeterAmpFail + 1
                    Label2.Text = "Reference meter Amps Error ! Comm. Count=" + Str(RefMeterVoltFail)
                    If RefMeterAmpFail > 50 Then
                        DisableTimer()
                        'MsgBox("Reference meter Amps Error ! Reinitialising the Source.")
                        For i = 0 To 6
                            InitCount(i) = 0
                            FillDataGridBlank(i)
                            Sample(i) = 0
                        Next
                        RefMeterAmpFail = 0
                        SNGTimer.Interval = 37000
                        If SourceType = 1 Then
                            SourceAxWinsock1.SendData("A")
                            Thread.Sleep(1000)
                            SourceAxWinsock1.SendData("A")
                            Thread.Sleep(1000)
                            TempData = "v1" + Trim(Str(CalVolt(CalStep))) + " i1" + Trim(Str(CalAmps(CalStep))) + " p1" + Trim(Str(CalPFDeg(CalStep))) + " s"
                            Label4.Text = "Calibration Step1: Volt=" + Str(CalVolt(CalStep)) + " Amps=" + Str(CalAmps(CalStep)) + " PF=" + Str(CalPF(CalStep))
                            ErrorEvents(Label4.Text)
                            SourceAxWinsock1.SendData(TempData)
                        Else
                            Set_APC8000(CalVolt(CalStep), CalAmps(CalStep), CalPF(CalStep))
                        End If

                        Thread.Sleep(35000)
                        EnableTimer()
                        Exit Sub
                    End If
                Else
                    Label14.Text = ""
                    RefMeterAmpFail = 0
                End If
            Else
                TempVolt = MeterData(0, VerfStep, 21) * 0.02
                TempAmps = MeterData(0, VerfStep, 25) * 0.02
                If VerVolt(VerfStep) > (MeterData(0, VerfStep, 21) + TempVolt) Or VerVolt(VerfStep) < (MeterData(0, VerfStep, 21) - TempVolt) Then
                    RefMeterVoltFail = RefMeterVoltFail + 1
                    Label14.Text = "Reference meter voltage Error ! Comm. Count=" + Str(RefMeterVoltFail)
                    If RefMeterVoltFail > 50 Then
                        DisableTimer()
                        'MsgBox("Reference meter Volt Error ! Reinitialising the Source.")
                        For i = 0 To 6
                            InitCount(i) = 0
                            FillDataGridBlank(i)
                            Sample(i) = 0
                        Next
                        RefMeterVoltFail = 0

                        SNGTimer.Interval = 37000
                        If SourceType = 1 Then
                            SourceAxWinsock1.SendData("A")
                            Thread.Sleep(1000)
                            SourceAxWinsock1.SendData("A")
                            Thread.Sleep(1000)
                            TempData = "v1" + Trim(Str(VerVolt(VerfStep))) + " i1" + Trim(Str(VerAmps(VerfStep))) + " p1" + Trim(Str(VerPFDeg(VerfStep))) + " s"
                            ErrorEvents(Label4.Text)
                            SourceAxWinsock1.SendData(TempData)
                        Else
                            Set_APC8000(VerVolt(VerfStep), VerAmps(VerfStep), VerPF(VerfStep))
                        End If
                        Thread.Sleep(35000)
                        EnableTimer()
                        Exit Sub
                    End If
                Else
                    Label14.Text = ""
                    RefMeterVoltFail = 0
                End If
                If VerAmps(VerfStep) > (MeterData(0, VerfStep, 25) + TempAmps) Or VerAmps(VerfStep) < (MeterData(0, VerfStep, 25) - TempAmps) Then
                    RefMeterAmpFail = RefMeterAmpFail + 1
                    Label2.Text = "Reference meter Amps Error ! Comm. Count=" + Str(RefMeterVoltFail)
                    If RefMeterAmpFail > 50 Then
                        DisableTimer()
                        For i = 0 To 6
                            InitCount(i) = 0
                            FillDataGridBlank(i)
                            Sample(i) = 0
                        Next
                        RefMeterAmpFail = 0
                        DisableTimer()
                        SNGTimer.Interval = 37000
                        If SourceType = 1 Then
                            SourceAxWinsock1.SendData("A")
                            Thread.Sleep(1000)
                            SourceAxWinsock1.SendData("A")
                            Thread.Sleep(1000)
                            TempData = "v1" + Trim(Str(VerVolt(VerfStep))) + " i1" + Trim(Str(VerAmps(VerfStep))) + " p1" + Trim(Str(VerPFDeg(VerfStep))) + " s"
                            ErrorEvents(Label4.Text)
                            SourceAxWinsock1.SendData(TempData)
                        Else
                            Set_APC8000(VerVolt(VerfStep), VerAmps(VerfStep), VerPF(VerfStep))
                        End If
                        Thread.Sleep(35000)
                        EnableTimer()
                        Exit Sub
                    End If
                Else
                    Label14.Text = ""
                    RefMeterAmpFail = 0
                End If
            End If
        End If

        If MeterNo = 0 Then GoTo 10

        If PollType = 0 Then
            If CCWrite(MeterNo) = 3 Then Exit Sub
            ReDim Data(5)
            SendSring = "010300000014"
            MeterRxBytes(MeterNo) = 45
            CCWrite(MeterNo) = CCWrite(MeterNo) + 1
            'ElseIf PollType = 1 Then
            '    If CCWrite(MeterNo) = 3 Then Exit Sub
            '    'ReDim Data(102)
            '    ReDim Data(394)
            '    'SendSring = "0110AA551B580002025000A025000020250000803FCDCC4C3E000000000000000000000000000000000000000000000000000000000000F07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07F000000000000000000000000000000000000000000000000"
            '    SendSring =
            '"0110AA551B580002040000A040000020400000803F0000003F0000803E000000000000000000000000000000000000000000" &
            '"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" &
            '"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" &
            '"0000000000F07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF0" &
            '"7FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF0" &
            '"7FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07FF07F00000000000000000000000000000000000000000000000000" &
            '"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" &
            '"000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

            '    'MeterRxBytes(MeterNo) = 10
            '    'CCWrite(MeterNo) = CCWrite(MeterNo) + 1

            '    MeterRxBytes(MeterNo) = 10

            '    Dim PacketLength As Integer = SendSring.Length \ 2

            '    ErrorEvents(
            '        "ET600 PT1 BEFORE TX" &
            '        " | Time=" & Now.ToString("yyyy-MM-dd HH:mm:ss.fff") &
            '        " | Meter=" & MeterNo &
            '        " | Attempt=" & (CCWrite(MeterNo) + 1) &
            '        " | PacketBytesBeforeCRC=" & PacketLength &
            '        " | ExpectedRXBytes=" & MeterRxBytes(MeterNo) &
            '        " | TX=" & SendSring
            '    )

            '    CCWrite(MeterNo) = CCWrite(MeterNo) + 1
        ElseIf PollType = 1 Then

            If CCWrite(MeterNo) >= 3 Then Exit Sub

            ' ET600 Default Calibration Constants Write
            ' Total packet: 397 bytes including CRC
            ' CRC expected: 0F94

            SendSring =
                "0110AA551B580002040000A040000020400000803F0000003F0000803E00" &
                New String("0"c, 250) &
                String.Concat(Enumerable.Repeat("F07F", 60)) &
                New String("0"c, 240)

            ' 395 bytes before CRC
            ReDim Data(394)

            If SendSring.Length <> 790 Then
                ErrorEvents("ET600 PT1 Invalid Packet Length")
                Exit Sub
            End If

            Dim PacketBytes() As Byte =
                Enumerable.Range(0, SendSring.Length \ 2).
                Select(Function(i) Convert.ToByte(SendSring.Substring(i * 2, 2), 16)).
                ToArray()

            Dim CalculatedCRC As String = CRC16(PacketBytes)

            ErrorEvents("ET600 PT1 | Meter=" & MeterNo & " | Attempt=" & (CCWrite(MeterNo) + 1) & " | TXBytes=" & (PacketBytes.Length + 2) & " | CRC=" & CalculatedCRC)

            If CalculatedCRC <> "0F94" Then
                ErrorEvents("ET600 PT1 CRC MISMATCH - WRITE BLOCKED")
                Exit Sub
            End If

            MeterRxBytes(MeterNo) = 10

            CCWrite(MeterNo) = CCWrite(MeterNo) + 1

        ElseIf PollType = 2 Then
            If CCWrite(MeterNo) = 3 Then Exit Sub
            ReDim Data(7)
            SendSring = "0103AA551B580002"
            'MeterRxBytes(MeterNo) = 101
            MeterRxBytes(MeterNo) = 393
            CCWrite(MeterNo) = CCWrite(MeterNo) + 1

        ElseIf PollType = 3 Then

            '=========================================================
            ' ET600 3P DEFAULT SETUP WRITE
            '
            ' CT Ratio = 5/5
            ' Requirement Excel:
            ' Model = ET600 / ET1200 3Ph
            '
            ' Full Excel string ends with CRC = 28 79
            ' Do NOT include CRC here because CRC16() adds it.
            '=========================================================

            If CCWrite(MeterNo) = 3 Then Exit Sub

            ' 107 bytes before CRC
            ' Array index 0 to 106
            ReDim Data(106)                              ' MODIFIED

            SendSring =
        "01101AF400326400004000800043CF800043CF000040A0" &
        "000040A0000040A0000040A0000040A0000040A0000040A0" &
        "000040A00000400000003F800000000000003F80000040800000" &
        "00000000447A00000000CCCD3ECC0000412000000000000040A0" &
        "0000000000000000"                       ' NEW - ET600 3P

            ' Function 10 write response is expected as 8 bytes
            MeterRxBytes(MeterNo) = 8                   ' MODIFIED

            CCWrite(MeterNo) = CCWrite(MeterNo) + 1

            'End If

        ElseIf PollType = 4 Then

            '=========================================================
            ' ET600 - DISPATCH / METER SERIAL NUMBER WRITE
            '
            ' Excel:
            ' Meter Serial No = 40009
            ' Data type       = Unsigned Long
            '
            ' Protocol address:
            ' 40009 -> offset 0008
            '
            ' Write 2 registers = 4 bytes
            '=========================================================

            If CCWrite(MeterNo) = 3 Then Exit Sub


            '=========================================================
            ' DO NOT WRITE ZERO SERIAL NUMBER
            '=========================================================
            If DispatchSlNo(MeterNo) = 0 Then

                ErrorEvents("ET600 Dispatch Serial Number is ZERO. Meter=" & MeterNo)
                ListBox1.Items.Add("Meter" & MeterNo & " Dispatch Serial Number not available")

                Exit Sub

            End If


            '=========================================================
            ' CONVERT UNSIGNED LONG TO 8-DIGIT HEX
            '
            ' Example:
            ' Decimal 12345678
            ' Hex     00BC614E
            '=========================================================
            Dim SerialHex As String = DispatchSlNo(MeterNo).ToString("X8")          ' NEW

            '=========================================================
            ' REQUEST:
            '
            ' 01    Slave
            ' 10    Function
            ' 0008  Start address
            ' 0002  Quantity
            ' 04    Byte count
            ' XXXXXXXX Serial number
            '
            ' Total BEFORE CRC = 11 bytes
            ' Therefore Data(0..10)
            '=========================================================
            ReDim Data(10)                                    ' NEW

            SendSring = "01100008000204" & SerialHex    ' NEW

            ' Standard Function 10 response = 8 bytes
            MeterRxBytes(MeterNo) = 8                         ' NEW

            CCWrite(MeterNo) = CCWrite(MeterNo) + 1

            ErrorEvents("ET600 Dispatch Serial Write Meter=" & MeterNo & " Serial=" & DispatchSlNo(MeterNo) & " TX=" & SendSring)

            'End If
        ElseIf PollType = 5 Then
            If CCWrite(MeterNo) = 3 Then Exit Sub
            ReDim Data(7)
            SendSring = "01051AC2FF002ADE"
            MeterRxBytes(MeterNo) = 8
            CCWrite(MeterNo) = CCWrite(MeterNo) + 1
            '        

        ElseIf PollType = 6 Then

10:         If MeterNo = 0 Then

                SendSring = "F7030064003C"

                MeterRxBytes(MeterNo) = 125

                ReDim Data(5)

            Else

                If ET600ReadWaiting(MeterNo) = True Then       ' NEW

                    ET600ReadTimeout(MeterNo) = ET600ReadTimeout(MeterNo) + 1


                    ' Give the response a few timer cycles to arrive
                    If ET600ReadTimeout(MeterNo) < 5 Then
                        Exit Sub
                    End If

                    ' Timeout - allow request to be retried
                    ET600ReadWaiting(MeterNo) = False
                    ET600ReadTimeout(MeterNo) = 0
                    rxBuff(MeterNo) = ""

                End If


                If ET600ReadStep(MeterNo) = 0 Then

                    SendSring = "01030FA00064"                 ' NEW

                    MeterRxBytes(MeterNo) = 205

                    ReDim Data(5)


                    '=====================================================
                    ' ET600 READ STEP 1
                    ' CH1 BLOCK
                    '
                    ' Sheet address: 4101/100
                    ' Actual Modbus offset = 4100 decimal
                    ' 4100 decimal = 1004 hex
                    '=====================================================

                ElseIf ET600ReadStep(MeterNo) = 1 Then

                    SendSring = "010310040064"                ' ET600 CH1-CH3

                    MeterRxBytes(MeterNo) = 205

                    ReDim Data(5)

                    ' ET600 READ STEP 2
                    '
                    ' 4201/100
                    ' Physical current channels:
                    ' R = CH4
                    ' Y = CH5
                    ' B = CH6
                    '
                    ' protocol offset 4200 = 1068
                ElseIf ET600ReadStep(MeterNo) = 2 Then

                    SendSring = "010310680064"                 ' NEW - ET600 CH4-CH6

                    MeterRxBytes(MeterNo) = 205

                    ReDim Data(5)

                End If


                ' Response now expected
                ET600ReadWaiting(MeterNo) = True               ' NEW

                ET600ReadTimeout(MeterNo) = 0                  ' NEW

            End If
            'ElseIf PollType = 7 Then
            '    If MeterNo > 0 Then
            '        If CCWrite(MeterNo) = 3 Then Exit Sub
            '        ReDim Data(102)
            '        SendSring = CalConstantStr(MeterNo)
            '        MeterRxBytes(MeterNo) = 10
            '        CCWrite(MeterNo) = CCWrite(MeterNo) + 1
            '    End If
        ElseIf PollType = 7 Then

            If MeterNo > 0 Then

                If CCWrite(MeterNo) = 3 Then Exit Sub


                '=====================================================
                ' ET600 FINAL CAL CONSTANT WRITE
                '
                ' CalConstantStr = 395 bytes BEFORE CRC
                '
                ' Array:
                ' Data(0) through Data(394)
                '=====================================================

                ReDim Data(394)                           ' MODIFIED


                SendSring = CalConstantStr(MeterNo)



                '=====================================================
                ' SAFETY CHECK
                '=====================================================

                If (Len(SendSring) \ 2) <> 395 Then

                    ErrorEvents("ET600 PollType7 Invalid Packet Length Meter=" & MeterNo & " Length=" & (Len(SendSring) \ 2))

                    Exit Sub

                End If

                ' Write acknowledgement
                MeterRxBytes(MeterNo) = 10

                CCWrite(MeterNo) = CCWrite(MeterNo) + 1


            End If
        ElseIf PollType = 8 Then

            '=========================================================
            ' ET600 3P POST-VERIFICATION SETUP WRITE
            '
            ' CT Ratio = 50/5
            '
            ' Workbook exact packet:
            ' 01 10 1A F4 00 32 64 ... 9F 52
            '
            ' CRC 9F52 is NOT included here.
            ' Existing CRC16() will append it.
            '=========================================================

            If MeterNo > 0 Then

                If CCWrite(MeterNo) = 3 Then Exit Sub

                ' 107 bytes BEFORE CRC
                ReDim Data(106)                              ' NEW

                SendSring =
                    "01101AF400326400004000800043CF800043CF" &
                    "00004248000040A000004248000040A00000" &
                    "4248000040A000004248000040A000004000" &
                    "00003F800000000000003F80000040800000" &
                    "00000000447A00000000CCCD3ECC00004120" &
                    "00000000000040A00000000000000000"     ' NEW

                ' Function 10 response = 8 bytes
                MeterRxBytes(MeterNo) = 8

                CCWrite(MeterNo) =
                    CCWrite(MeterNo) + 1

                ErrorEvents("ET600 CT 50/5 WRITE Meter=" & MeterNo & " TX=" & SendSring)

            End If
        Else
            Exit Sub
        End If

        If MeterNo = 0 Then
            PollPkts0(SendSring)
        ElseIf MeterNo = 1 Then
            PollPkts1("TX: " + Str(PollType) + " " + SendSring + "," + Now.ToString("dd-MM-yy HH:mm"))
        ElseIf MeterNo = 2 Then
            PollPkts2(SendSring)
        ElseIf MeterNo = 3 Then
            PollPkts3(SendSring)
        ElseIf MeterNo = 4 Then
            PollPkts4(SendSring)
        ElseIf MeterNo = 5 Then
            PollPkts5(SendSring)
        ElseIf MeterNo = 6 Then
            PollPkts6(SendSring)
        ElseIf MeterNo = 7 Then
            PollPkts7(SendSring)
        ElseIf MeterNo = 8 Then
            PollPkts8(SendSring)
        ElseIf MeterNo = 9 Then
            PollPkts9(SendSring)
        ElseIf MeterNo = 10 Then
            PollPkts10(SendSring)
        ElseIf MeterNo = 11 Then
            PollPkts11("TX: " + Str(PollType) + " " + SendSring + "," + Now.ToString("dd-MM-yy HH:mm"))
        ElseIf MeterNo = 12 Then
            PollPkts12(SendSring)
        ElseIf MeterNo = 13 Then
            PollPkts13(SendSring)
        ElseIf MeterNo = 14 Then
            PollPkts14(SendSring)
        ElseIf MeterNo = 15 Then
            PollPkts15(SendSring)
        ElseIf MeterNo = 16 Then
            PollPkts16(SendSring)
        ElseIf MeterNo = 17 Then
            PollPkts17(SendSring)
        ElseIf MeterNo = 18 Then
            PollPkts18(SendSring)
        End If

        MeterReadFail(MeterNo) = MeterReadFail(MeterNo) + 1
        If MeterReadFail(MeterNo) = 200 Then
            MeterReadFail(MeterNo) = 20
        End If

        Data_send = ""
        j = 0
        For i = 1 To Len(SendSring) Step 2
            Data_send = Data_send + Chr(Val("&h" + Mid(SendSring, i, 2)))
            Data(j) = CByte("&h" + Mid(SendSring, i, 2))
            j = j + 1
        Next

        CRC = CRC16(Data)
        Data_send = Data_send + Chr(Val("&h" + Mid(CRC, 1, 2))) + Chr(Val("&h" + Mid(CRC, 3, 2)))
        rxBuff(MeterNo) = ""

        If MeterNo = 0 Then
            WS_State = AxWinsock0.CtlState
            If WS_State = 7 Then
                ErrorEvents(Str(MeterNo) + " connected")
                AxWinsock0.SendData(Data_send)
                AxWinsock0.GetData(comBuffer)
            Else
                SocketConnect(0)
            End If
        ElseIf MeterNo = 1 Then
            WS_State = AxWinsock1.CtlState
            If WS_State = 7 Then
                ErrorEvents(Str(MeterNo) + " connected")
                If PollType = 1 Then
                    ErrorEvents("ET600 PT1 SOCKET SEND" & " | Meter=" & MeterNo & " | SocketState=" & AxWinsock1.CtlState & " | TXBytesWithCRC=" & (SendSring.Length \ 2) + 2 & " | Time=" & Now.ToString("HH:mm:ss.fff"))
                End If
                AxWinsock1.SendData(Data_send)
                AxWinsock1.GetData(comBuffer)
            Else
                SocketConnect(1)
            End If
        ElseIf MeterNo = 2 Then
            WS_State = AxWinsock2.CtlState
            If WS_State = 7 Then
                ErrorEvents(Str(MeterNo) + " connected")
                AxWinsock2.SendData(Data_send)
                AxWinsock2.GetData(comBuffer)
            Else
                SocketConnect(2)
            End If
        ElseIf MeterNo = 3 Then
            WS_State = AxWinsock3.CtlState
            If WS_State = 7 Then
                ErrorEvents(Str(MeterNo) + " connected")
                AxWinsock3.SendData(Data_send)
                AxWinsock3.GetData(comBuffer)
            Else
                SocketConnect(3)
            End If
        ElseIf MeterNo = 4 Then
            WS_State = AxWinsock4.CtlState
            If WS_State = 7 Then
                ErrorEvents(Str(MeterNo) + " connected")
                AxWinsock4.SendData(Data_send)
                AxWinsock4.GetData(comBuffer)
            Else
                SocketConnect(4)
            End If
        ElseIf MeterNo = 5 Then
            WS_State = AxWinsock5.CtlState
            If WS_State = 7 Then
                ErrorEvents(Str(MeterNo) + " connected")
                AxWinsock5.SendData(Data_send)
                AxWinsock5.GetData(comBuffer)
            Else
                SocketConnect(5)
            End If
        ElseIf MeterNo = 6 Then
            WS_State = AxWinsock6.CtlState
            If WS_State = 7 Then
                ErrorEvents(Str(MeterNo) + " connected")
                AxWinsock6.SendData(Data_send)
                AxWinsock6.GetData(comBuffer)
            Else
                SocketConnect(6)
            End If

        End If

        'Catch ex As Exception
        '    MsgBox("PollMeters : " + ex.Message)
        'End Try

    End Sub


    Function dec_to_float(ByVal dec As Decimal) As String
        Dim d1 As Decimal = 0
        Dim d2 As Decimal = 0
        Dim d3 As Decimal = 0
        Dim d4 As Decimal = 0

        Dim i1 As Integer = 0
        Dim i2 As Integer = 0
        Dim i3 As Integer = 0
        Dim i4 As UInteger = 0
        Dim i5 As UInteger = 0
        Dim i6 As UInteger = 0
        Dim i7 As UInteger = 0
        'Dim i8 As UInteger = 0

        Dim s1 As String = ""
        Dim s2 As String = ""
        Dim s3 As String = ""
        Dim s4 As String = ""
        Try

            If dec < 0 Then 'evaluate sign bit
                i7 = 2 ^ 31
                dec = -dec
            End If
            '******************************************************
            If dec <> 0 Then

                '******************************************************
                d1 = dec
                If d1 >= 2 Then
                    Do Until d1 < 2
                        d1 = d1 / 2
                        i1 = i1 + 1
                    Loop

                ElseIf d1 < 1 Then
                    Do Until d1 >= 1
                        d1 = d1 * 2
                        i1 = i1 - 1
                    Loop

                End If
                i1 = i1 + 127 'exponent
                '******************************************************

                d2 = d1
                d3 = Math.Floor(d2)
                Do Until (d3 = d2) Or (d2 > 4294967295) '21474836484294967295 (i3 >= 23) 
                    d4 = Math.Floor(d2)
                    d2 = d2 * 2
                    d3 = Math.Floor(d2)
                    i3 = i3 + 1

                Loop

                If d3 > 4294967295 Then
                    i4 = Convert.ToUInt32(d4)
                Else
                    i4 = Convert.ToUInt32(d3)
                End If

                Do Until (i4 And 2 ^ 31) = 2 ^ 31
                    i4 = i4 << 1
                Loop

                i5 = (i4 << 1)
                i5 = (i5 >> 9) 'mantissa bits 22 -> 0
                '*********************************************************

                i6 = (i1 << 23) + i5 'exponent + mantissa
                i6 = i6 + i7 'sign + exponent + mantissa

                s4 = i6.ToString("X8") ' end result

            ElseIf dec = 0 Then
                'i6 = i6 + i7
                s4 = i6.ToString("X8") ' end result

            End If

        Catch ex As Exception

        End Try

        Return s4

    End Function

    Private Sub ErrorEvents(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\LogEvents.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub

    Private Sub PollPkts0(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts0.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub

    Private Sub PollPkts1(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts1.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub

    Private Sub PollPkts2(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts2.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts11(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts11.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub

    Private Sub PollPkts12(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts12.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts5(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts5.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts15(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts15.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts6(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts6.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts7(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts7.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts8(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts8.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts16(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts16.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts17(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts17.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts18(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts18.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts9(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts9.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts10(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts10.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts3(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts3.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts4(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts4.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts13(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts13.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub PollPkts14(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\PollPkts14.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub ErrorEvents2(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\ErrLogs.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub ErrorEvents4(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\CalConst" + Trim(Str(Calno)) + ".CSV"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub ErrorEvents1(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\LogPkt.txt"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub
    Private Sub ErrorEvents3(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\Samples" + Trim(Str(Calno)) + ".CSV"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub

    'Private Sub Button1_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

    '    Dim WS_State As Short

    '    DeleteCalConstants()
    '    ReDim CCWrite(6)
    '    WS_State = SourceAxWinsock1.CtlState
    '    If WS_State = 7 Then
    '        CalStep = 1
    '        ProcessType = 1
    '        SNG_Step = 1
    '        SNGTimer.Enabled = True
    '        Button1.Enabled = False
    '        EnableMeterTimer()
    '        PollType = 6
    '    End If

    'End Sub

    Private Sub Button1_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        Dim WS_State As Short

        DeleteCalConstants()
        ReDim CCWrite(6)

        WS_State = SourceAxWinsock1.CtlState

        If WS_State = 7 Then

            CalStep = 1
            ProcessType = 1
            PollType = 6
            SNG_Step = 1

            For i = 0 To NoOfMUT
                Sample(i) = 0
                InitCount(i) = 0
            Next

            For i = 1 To NoOfMUT
                ET600ReadStep(i) = 0
                ET600ReadWaiting(i) = False
                ET600ReadTimeout(i) = 0
                rxBuff(i) = ""
            Next

            Button1.Enabled = False
            Label4.Text = "Starting ET600 Calibration Step 1"

            EnableMeterTimer()

            SNGTimer.Interval = 2000
            SNGTimer.Enabled = True

        End If

    End Sub

    Function Check_RWComplete()
        Dim RW_Complete As Byte
        For i = 1 To NoOfMUT
            RW_Complete = 0
            If MeterSACardNo(i) = 0 Then
                CCWrite(i) = 3
            End If
            If CCWrite(i) >= 3 Then
                RW_Complete = 1
            End If
        Next
        Check_RWComplete = RW_Complete
    End Function

    'Function Check_Sample(ByVal SCount As Byte)
    '    Dim RW_Complete As Byte
    '    For i = 1 To NoOfMUT
    '        RW_Complete = 0
    '        If Sample(i) >= SCount Then
    '            RW_Complete = 1
    '        End If
    '    Next
    '    Check_Sample = RW_Complete
    'End Function

    Function Check_Sample(ByVal SCount As Byte) As Byte

        For i = 1 To NoOfMUT
            If Sample(i) < SCount Then
                Return 0
            End If
        Next

        Return 1

    End Function

    Function Check_MaxSample(ByVal SCount As Byte)
        Dim RW_Complete As Byte
        For i = 1 To NoOfMUT
            RW_Complete = 0
            If Sample(i) >= SCount Or Sample(0) >= SCount Then
                RW_Complete = 1
                Exit For
            End If
        Next
        Check_MaxSample = RW_Complete

    End Function

    Private Sub SlNoDataGrid()

        For i = 1 To NoOfMUT
            DataGridView1.Item(0, i).Value = MeterSACardNo(i)
        Next

    End Sub

    Private Sub VerficationData(ByVal ErrMsg As String)

        Dim MyPath As String
        MyPath = My.Application.Info.DirectoryPath + "\VerficationData_" + Trim(Str(Calno)) + ".CSV"
        Using writer As StreamWriter = New StreamWriter(MyPath, True)
            writer.WriteLine(ErrMsg)
        End Using

    End Sub


    Private Sub SNGTimer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SNGTimer.Tick

        Dim TempData As String

        If SNG_Step = 200 Then Exit Sub

        If SNG_Step = 0 And PollType = 0 And SourceType = 1 Then
            SourceAxWinsock1.SendData("h")
        ElseIf SNG_Step = 1 And PollType = 0 And SourceType = 1 Then
            SourceAxWinsock1.SendData("A")
        ElseIf SNG_Step = 2 And PollType = 0 And SourceType = 1 Then
            SourceAxWinsock1.SendData("A")
        ElseIf SNG_Step = 3 And PollType = 0 And SourceType = 1 Then
            SourceAxWinsock1.SendData("A")
        ElseIf SNG_Step = 4 And PollType = 0 And SourceType = 1 Then
            SNGTimer.Interval = 10000
            TempData = "v1240 i10 p10 s"
            Label4.Text = "Calibration Volt = 240"
            ErrorEvents(Label4.Text)
            SourceAxWinsock1.SendData(TempData)
        Else
            SNGTimer.Interval = 2000
        End If

        If SNG_Step <= 4 And PollType = 0 And SourceType = 1 Then
            SNG_Step = SNG_Step + 1
            Exit Sub
        End If

        If PollType = 0 Then
            If Check_RWComplete() = 1 Then
                SlNoDataGrid()
                PollType = 1
                ReDim CCWrite(6)
            End If
            Exit Sub
        ElseIf PollType = 7 Then
            If Check_RWComplete() = 1 Then
                PollType = 6
                ProcessType = 2
                Label4.Text = "CC Write Completed.."
                ReDim CCWrite(6)
                For i = 0 To 6
                    Sample(i) = 0
                Next
            End If
            Exit Sub
        ElseIf PollType = 1 Then
            If Check_RWComplete() = 1 Then
                ReDim CCWrite(6)
                PollType = 2
            End If
            Exit Sub
        ElseIf PollType = 2 Then
            If Check_RWComplete() = 1 Then
                ReDim CCWrite(6)
                PollType = 3
            End If
            Exit Sub
            'ElseIf PollType = 3 Then
            '    If Check_RWComplete() = 1 Then
            '        ReDim CCWrite(6)
            '        PollType = 4
            '    End If
            '    Exit Sub
        ElseIf PollType = 3 Then

            If Check_RWComplete() = 1 Then

                '=====================================================
                ' ET600 DEFAULT SETUP 5/5 COMPLETED
                '
                ' DISPATCH SERIAL WRITE IS SKIPPED FOR NOW
                '
                ' NEXT:
                ' USER WILL START CALIBRATION MANUALLY
                '=====================================================

                ReDim CCWrite(6)

                DisableTimer()

                SNGTimer.Enabled = False

                Label4.Text = "ET600 Initialization Completed - Ready for Calibration"
                ErrorEvents("ET600 Default Setup CT 5/5 Completed. " & "Dispatch Serial Write skipped. " & "Ready for Calibration.")

                Button1.Enabled = True

                MsgBox("ET600 Initialization Completed." & vbCrLf & "Default DCC Write : Completed" & vbCrLf & "DCC Read : Completed" & vbCrLf & "Setup CT 5/5 : Completed" & vbCrLf & "Dispatch Serial : Skipped" & vbCrLf & vbCrLf & "Click Start Calibration.")

            End If

            Exit Sub
            'ElseIf PollType = 4 Then
            '    If Check_RWComplete() = 1 Then
            '        ReDim CCWrite(6)
            '        PollType = 5
            '    End If
            '    Exit Sub


        ElseIf PollType = 5 Then
            If Check_RWComplete() = 1 Then
                ReDim CCWrite(6)
                SNGTimer.Enabled = False
                DisableTimer()
                MsgBox("Start Calibration..")
                Button1.Enabled = True
            End If
            Exit Sub
        ElseIf PollType = 8 Then

            If Check_RWComplete() = 1 Then

                '=====================================================
                ' ET600 CT RATIO 50/5 WRITE COMPLETED
                '=====================================================

                DisableTimer()

                SNGTimer.Enabled = False

                ReDim CCWrite(6)
                Label4.Text = "ET600 CT Ratio 50/5 Write Completed"

                ErrorEvents("ET600 CT Ratio 50/5 Write Completed for all MUTs")

                MsgBox("ET600 Verification Completed." & vbCrLf & "CT Ratio 50/5 Write Completed." & vbCrLf & vbCrLf & "Next Step: RTC Write.")

            End If

            Exit Sub
        End If

        SNG_Step = SNG_Step + 1
        Label1.Text = Str(SNG_Step) + " ProcessT=" + Str(ProcessType) + " PollT=" + Str(PollType)

        If ProcessType = 1 Then
            If SourceType = 1 Then
                If SNG_Step = 1 Then
                    'SourceAxWinsock1.SendData("h")
                    'ErrorEvents("h")
                ElseIf SNG_Step = 2 Then
                    SourceAxWinsock1.SendData("A")
                ElseIf SNG_Step = 3 Then
                    SourceAxWinsock1.SendData("A")
                ElseIf SNG_Step = 4 Then
                    SourceAxWinsock1.SendData("A")
                ElseIf SNG_Step = 5 Then
                    ReDim InitCount(6)
                    ReDim MeterData(6, 7, 29)
                    SNGTimer.Interval = 25000
                    TempData = "v1" + Trim(Str(CalVolt(CalStep))) + " i1" + Trim(Str(CalAmps(CalStep))) + " p1" + Trim(Str(CalPFDeg(CalStep))) + " s"
                    Label4.Text = "Calibration Step: Volt=" + Str(CalVolt(CalStep)) + " Amps=" + Str(CalAmps(CalStep)) + " PF=" + Str(CalPF(CalStep))
                    ErrorEvents(Label4.Text)
                    SourceAxWinsock1.SendData(TempData)
                    Thread.Sleep(25000)
                ElseIf SNG_Step > 6 Then
                    EnableTimer()
                    SNGTimer.Interval = 2000
                    If SNG_Step = 50 Then SNG_Step = 6
                End If
            Else
                If SNG_Step = 5 Then
                    ReDim InitCount(6)
                    ReDim MeterData(6, 7, 29)
                    'SNGTimer.Interval = 25000
                    'Set_APC8000(CalVolt(CalStep), CalAmps(CalStep), CalPF(CalStep))
                    'Label4.Text = "Calibration Step:" + Str(CalStep) + "  Volt=" + Str(CalVolt(CalStep)) + " Amps=" + Str(CalAmps(CalStep)) + " PF=" + Str(CalPF(CalStep))
                    'ErrorEvents(Label4.Text)
                    'Thread.Sleep(25000)
                    If CalStep = 5 Then
                        SNGTimer.Interval = 60000
                    Else
                        SNGTimer.Interval = 50000
                    End If

                    Set_APC8000(CalVolt(CalStep), CalAmps(CalStep), CalPF(CalStep))

                    Label4.Text = "Calibration Step:" + Str(CalStep) + "  Volt=" + Str(CalVolt(CalStep)) + " Amps=" + Str(CalAmps(CalStep)) + " PF=" + Str(CalPF(CalStep))
                    ErrorEvents(Label4.Text)

                    If CalStep = 5 Then
                        Thread.Sleep(60000)
                    Else
                        Thread.Sleep(50000)
                    End If


                ElseIf SNG_Step > 6 Then
                    EnableMeterTimer()

                    SNGTimer.Interval = 1000
                    If SNG_Step = 50 Then SNG_Step = 6
                End If
            End If

            If Check_Sample(15) = 1 Then
                SNG_Step = 200
                DisableTimer()
                FindAVeCalData()
                SNG_Step = 1
                CalStep = CalStep + 1
                For i = 0 To 6
                    InitCount(i) = 0
                    FillDataGridBlank(i)
                    Sample(i) = 0
                Next
                If CalStep = 6 Then
                    FindCalConstant()
                    VerfStep = 1
                    PollType = 7
                    ReDim CCWrite(6)
                    ProcessType = 2
                End If
                EnableMeterTimer()
            ElseIf Check_MaxSample(20) = 1 Then
                SNG_Step = 200
                DisableTimer()
                FindAVeCalData()
                SNG_Step = 1
                CalStep = CalStep + 1
                For i = 0 To 6
                    InitCount(i) = 0
                    FillDataGridBlank(i)
                    Sample(i) = 0
                Next
                If CalStep = 6 Then
                    FindCalConstant()
                    VerfStep = 1
                    PollType = 7
                    ReDim CCWrite(6)
                    ProcessType = 2
                End If
                EnableMeterTimer()
            End If
        End If



        If ProcessType = 2 Then

            '=========================================================
            ' ET600 VERIFICATION PROCESS
            '=========================================================

            If SourceType = 1 Then

                If SNG_Step = 1 Then

                    'SourceAxWinsock1.SendData("h")
                    'ErrorEvents("h")

                ElseIf SNG_Step = 2 Then

                    SourceAxWinsock1.SendData("A")

                ElseIf SNG_Step = 3 Then

                    SourceAxWinsock1.SendData("A")

                ElseIf SNG_Step = 4 Then

                    SourceAxWinsock1.SendData("A")

                ElseIf SNG_Step = 5 Then

                    '=================================================
                    ' START CURRENT VERIFICATION STEP
                    '=================================================

                    ReDim InitCount(6)                      ' MODIFIED - 6 MUT
                    ReDim MeterData(6, 7, 29)              ' MODIFIED - 6 MUT

                    SNGTimer.Interval = 25000

                    TempData = "v1" & Trim(Str(VerVolt(VerfStep))) & " i1" & Trim(Str(VerAmps(VerfStep))) & " p1" & Trim(Str(VerPFDeg(VerfStep))) & " s"

                    Label4.Text = "Verification Step:" & Str(VerfStep) & " Volt=" & Trim(Str(VerVolt(VerfStep))) & " Amps=" & Trim(Str(VerAmps(VerfStep))) & " PF=" & Trim(Str(VerPF(VerfStep)))

                    ErrorEvents(Str(VerfStep) & " " & TempData)

                    SourceAxWinsock1.SendData(TempData)

                    Thread.Sleep(25000)

                ElseIf SNG_Step > 6 Then

                    EnableTimer()

                    SNGTimer.Interval = 2000

                    If SNG_Step = 50 Then
                        SNG_Step = 6
                    End If

                End If


            Else

                '=====================================================
                ' APC SOURCE
                '=====================================================

                If SNG_Step = 2 Then

                    ReDim MeterData(6, 7, 29)              ' MODIFIED
                    ReDim InitCount(6)                     ' MODIFIED

                    SNGTimer.Interval = 25000

                    Set_APC8000(VerVolt(VerfStep), VerAmps(VerfStep), VerPF(VerfStep))

                    Label4.Text = "Verification Step:" & Str(VerfStep) & " Volt=" & Trim(Str(VerVolt(VerfStep))) & " Amps=" & Trim(Str(VerAmps(VerfStep))) & " PF=" & Trim(Str(VerPF(VerfStep)))

                    ErrorEvents(Str(VerfStep) & " " & Label4.Text)

                    Thread.Sleep(25000)

                ElseIf SNG_Step > 6 Then

                    EnableMeterTimer()

                    SNGTimer.Interval = 1000

                    If SNG_Step = 50 Then
                        SNG_Step = 6
                    End If

                End If

            End If


            '=========================================================
            ' VERIFICATION STEP COMPLETE
            '=========================================================

            If Check_Sample(15) = 1 Then

                SNGTimer.Enabled = False

                SNG_Step = 200

                DisableTimer()

                FindAVeVerData()

                SNG_Step = 1

                VerfStep = VerfStep + 1

                SNGTimer.Enabled = True


                '=====================================================
                ' RESET FOR NEXT VERIFICATION STEP
                '=====================================================

                For i = 0 To NoOfMUT

                    If Sample(i) < 10 Then
                        MeterStatus(i) = 0
                    Else
                        MeterStatus(i) = 1
                    End If

                    Sample(i) = 0
                    InitCount(i) = 0

                    FillDataGridBlank(i)

                Next


                If VerfStep <= 7 Then

                    ' Reset ET600 live read sequence
                    For i = 1 To NoOfMUT

                        ET600ReadStep(i) = 0
                        ET600ReadWaiting(i) = False
                        ET600ReadTimeout(i) = 0

                    Next

                    EnableMeterTimer()

                End If


            ElseIf Check_MaxSample(20) = 1 Then

                SNGTimer.Enabled = False

                SNG_Step = 200

                DisableTimer()

                FindAVeVerData()

                SNG_Step = 1

                VerfStep = VerfStep + 1

                SNGTimer.Enabled = True


                '=====================================================
                ' RESET FOR NEXT VERIFICATION STEP
                '=====================================================

                For i = 0 To NoOfMUT

                    If Sample(i) < 10 Then
                        MeterStatus(i) = 0
                    Else
                        MeterStatus(i) = 1
                    End If

                    Sample(i) = 0
                    InitCount(i) = 0

                    FillDataGridBlank(i)

                Next


                If VerfStep <= 7 Then

                    For i = 1 To NoOfMUT

                        ET600ReadStep(i) = 0
                        ET600ReadWaiting(i) = False
                        ET600ReadTimeout(i) = 0

                    Next

                    EnableMeterTimer()

                End If

            End If


            '=========================================================
            ' ALL 7 VERIFICATION STEPS COMPLETED
            '=========================================================

            If VerfStep = 8 Then

                '=====================================================
                ' SWITCH SOURCE OFF
                '=====================================================

                If SourceType = 1 Then

                    SourceAxWinsock1.SendData("v10 i10 p10 s")

                Else

                    Set_APC8000(0,0,1)

                End If

                Thread.Sleep(3000)

                DisableTimer()

                SNGTimer.Enabled = False


                '=====================================================
                ' VERIFICATION CSV HEADER
                '=====================================================

                VerficationData("Card No,Test No," & "VLN,Vr,Vy,Vb," & "AvgAmps,CH1,CH2,CH3,CH4,CH5,CH6," & "AvgPF,PF1,PF2,PF3,PF4,PF5,PF6,Hz")


                '=====================================================
                ' CALCULATE ET600 VERIFICATION ERRORS
                '=====================================================

                For i = 1 To NoOfMUT

                    ' Reset class before evaluation
                    AccClass(i) = 0


                    If MeterReadFail(i) < 10 Then


                        For k = 1 To 7

                            '=========================================
                            ' REFERENCE VALUES
                            '=========================================

                            Dim RefVolt As Double = AveVerDataSteps(0, 21, k)

                            Dim RefAmp As Double = AveVerDataSteps(0, 25, k)

                            Dim RefPFValue As Double = AveVerDataSteps(0, 9, k)

                            Dim RefHz As Double = AveVerDataSteps(0, 29, k)

                            '=========================================
                            ' VOLTAGE ERROR
                            '=========================================

                            ' VLN Average
                            VerErrData(i, 21, k) = CalcPercentError(AveVerDataSteps(i, 21, k), RefVolt)

                            ' Vr
                            VerErrData(i, 1, k) = CalcPercentError(AveVerDataSteps(i, 1, k), RefVolt)

                            ' Vy
                            VerErrData(i, 2, k) = CalcPercentError(AveVerDataSteps(i, 2, k), RefVolt)

                            ' Vb
                            VerErrData(i, 3, k) = CalcPercentError(AveVerDataSteps(i, 3, k), RefVolt)

                            '=========================================
                            ' CURRENT ERROR - ALL 6 CHANNELS
                            '=========================================

                            ' Average current
                            VerErrData(i, 25, k) = CalcPercentError(AveVerDataSteps(i, 25, k), RefAmp)

                            ' CH1
                            VerErrData(i, 4, k) = CalcPercentError(AveVerDataSteps(i, 4, k), RefAmp)

                            ' CH2
                            VerErrData(i, 5, k) = CalcPercentError(AveVerDataSteps(i, 5, k), RefAmp)

                            ' CH3
                            VerErrData(i, 6, k) = CalcPercentError(AveVerDataSteps(i, 6, k), RefAmp)

                            ' CH4
                            VerErrData(i, 15, k) = CalcPercentError(AveVerDataSteps(i, 15, k), RefAmp)

                            ' CH5
                            VerErrData(i, 16, k) = CalcPercentError(AveVerDataSteps(i, 16, k), RefAmp)

                            ' CH6
                            VerErrData(i, 17, k) = CalcPercentError(AveVerDataSteps(i, 17, k), RefAmp)

                            '=========================================
                            ' PF ERROR - ALL 6 CHANNELS
                            '
                            ' PF is difference, not percent error.
                            '=========================================

                            ' PF Average
                            VerErrData(i, 14, k) = Math.Abs(AveVerDataSteps(i, 14, k)) - Math.Abs(RefPFValue)

                            ' PF CH1
                            VerErrData(i, 7, k) = Math.Abs(AveVerDataSteps(i, 7, k)) - Math.Abs(RefPFValue)

                            ' PF CH2
                            VerErrData(i, 8, k) = Math.Abs(AveVerDataSteps(i, 8, k)) - Math.Abs(RefPFValue)

                            ' PF CH3
                            VerErrData(i, 9, k) = Math.Abs(AveVerDataSteps(i, 9, k)) - Math.Abs(RefPFValue)

                            ' PF CH4
                            VerErrData(i, 18, k) = Math.Abs(AveVerDataSteps(i, 18, k)) - Math.Abs(RefPFValue)

                            ' PF CH5
                            VerErrData(i, 19, k) = Math.Abs(AveVerDataSteps(i, 19, k)) - Math.Abs(RefPFValue)

                            ' PF CH6
                            VerErrData(i, 20, k) = Math.Abs(AveVerDataSteps(i, 20, k)) - Math.Abs(RefPFValue)

                            '=========================================
                            ' FREQUENCY ERROR
                            '=========================================

                            VerErrData(i, 29, k) = CalcPercentError(AveVerDataSteps(i, 29, k), RefHz)

                            '=========================================
                            ' ACCURACY CLASS
                            '
                            ' For now preserve existing thresholds
                            ' using average voltage/current.
                            '=========================================

                            'Dim VoltError As Double = Math.Abs(VerErrData(i, 21, k))

                            'Dim AmpError As Double = Math.Abs(VerErrData(i, 25, k))

                            Dim VoltError As Double = Math.Max(Math.Abs(VerErrData(i, 21, k)), Math.Max(Math.Abs(VerErrData(i, 1, k)), Math.Max(Math.Abs(VerErrData(i, 2, k)), Math.Abs(VerErrData(i, 3, k)))))
                            Dim AmpError As Double = Math.Max(Math.Abs(VerErrData(i, 25, k)), Math.Max(Math.Abs(VerErrData(i, 4, k)), Math.Max(Math.Abs(VerErrData(i, 5, k)), Math.Max(Math.Abs(VerErrData(i, 6, k)), Math.Max(Math.Abs(VerErrData(i, 15, k)), Math.Max(Math.Abs(VerErrData(i, 16, k)), Math.Abs(VerErrData(i, 17, k))))))))


                            '-----------------------------------------
                            ' FAIL >= 1%
                            '-----------------------------------------

                            If VoltError >= 1 OrElse
                               AmpError >= 1 Then

                                AccClass(i) = 4

                            Else

                                '-------------------------------------
                                ' CLASS BASED ON VOLTAGE
                                '-------------------------------------

                                If VoltError > 0.495 Then

                                    If AccClass(i) < 4 Then
                                        AccClass(i) = 1
                                    End If

                                ElseIf VoltError > 0.195 Then

                                    If AccClass(i) < 3 Then
                                        AccClass(i) = 2
                                    End If

                                ElseIf VoltError > 0 Then

                                    If AccClass(i) < 2 Then
                                        AccClass(i) = 3
                                    End If

                                End If


                                '-------------------------------------
                                ' CLASS BASED ON CURRENT
                                '-------------------------------------

                                If AmpError > 0.495 Then

                                    If AccClass(i) < 4 Then
                                        AccClass(i) = 1
                                    End If

                                ElseIf AmpError > 0.195 Then

                                    If AccClass(i) < 3 Then
                                        AccClass(i) = 2
                                    End If

                                ElseIf AmpError > 0 Then

                                    If AccClass(i) < 2 Then
                                        AccClass(i) = 3
                                    End If

                                End If

                            End If

                            '=========================================
                            ' IF PERFECT 0 ERROR, MARK BEST CLASS
                            '=========================================

                            If AccClass(i) = 0 Then
                                AccClass(i) = 3
                            End If

                            '=========================================
                            ' DEBUG LOG
                            '=========================================

                            ErrorEvents("ET600 VERIFY" & " Meter=" & i & " Step=" & k & " VErr=" & VerErrData(i, 21, k) & " AErr=" & VerErrData(i, 25, k) & " PFErr=" & VerErrData(i, 14, k) & " HzErr=" & VerErrData(i, 29, k) & " Class=" & AccClass(i))

                        Next

                    Else
                        AccClass(i) = 4
                    End If
                    '=================================================
                    ' WRITE ALL 7 VERIFICATION STEPS TO CSV
                    '=================================================

                    Dim TempData1 As String
                    For k = 1 To 7
                        TempData1 = Str(MeterSACardNo(i)) & "," & Str(k)

                        '---------------------------------------------
                        ' VOLTAGE
                        '---------------------------------------------

                        TempData1 &= "," & Str(VerErrData(i, 21, k))    ' VLN

                        TempData1 &= "," & Str(VerErrData(i, 1, k))     ' Vr

                        TempData1 &= "," & Str(VerErrData(i, 2, k))     ' Vy

                        TempData1 &= "," & Str(VerErrData(i, 3, k))     ' Vb

                        '---------------------------------------------
                        ' CURRENT
                        '---------------------------------------------

                        TempData1 &= "," & Str(VerErrData(i, 25, k))    ' Avg

                        TempData1 &= "," & Str(VerErrData(i, 4, k))     ' CH1

                        TempData1 &= "," & Str(VerErrData(i, 5, k))     ' CH2

                        TempData1 &= "," & Str(VerErrData(i, 6, k))     ' CH3

                        TempData1 &= "," & Str(VerErrData(i, 15, k))    ' CH4

                        TempData1 &= "," & Str(VerErrData(i, 16, k))    ' CH5

                        TempData1 &= "," & Str(VerErrData(i, 17, k))    ' CH6

                        '---------------------------------------------
                        ' PF
                        '---------------------------------------------

                        TempData1 &= "," & Str(VerErrData(i, 14, k))    ' Avg PF

                        TempData1 &= "," & Str(VerErrData(i, 7, k))     ' PF CH1

                        TempData1 &= "," & Str(VerErrData(i, 8, k))     ' PF CH2

                        TempData1 &= "," & Str(VerErrData(i, 9, k))     ' PF CH3

                        TempData1 &= "," & Str(VerErrData(i, 18, k))    ' PF CH4

                        TempData1 &= "," & Str(VerErrData(i, 19, k))    ' PF CH5

                        TempData1 &= "," & Str(VerErrData(i, 20, k))    ' PF CH6

                        '---------------------------------------------
                        ' FREQUENCY
                        '---------------------------------------------

                        TempData1 &= "," & Str(VerErrData(i, 29, k))

                        VerficationData(TempData1)

                    Next

                Next

                '=====================================================
                ' UPDATE GRID
                '=====================================================

                For i = 1 To NoOfMUT

                    FillErrDataGrid(i)

                Next


                '=====================================================
                ' EXISTING DATABASE CALLS
                '
                ' You said not to modify UpdateCalConstants().
                ' These verification database functions are also
                ' being left unchanged for now.
                '=====================================================

                UpdateAccuracy()

                UpdateErrData()


                '=====================================================
                ' ET600 VERIFICATION COMPLETED
                '
                ' NEXT STEP = CT RATIO 50/5 WRITE
                '=====================================================

                Label4.Text = "ET600 Verification Completed - Writing CT Ratio 50/5"

                ErrorEvents("ET600 7-Step Verification Completed. " & "Starting CT Ratio 50/5 Write.")

                ' Reset write counter for new PollType
                ReDim CCWrite(6)

                ' Move to ET600 CT Ratio 50/5 write
                PollType = 8

                ' Verification process is finished
                ProcessType = 0

                ' Reset ET600 communication states
                For i = 1 To NoOfMUT

                    ET600ReadStep(i) = 0
                    ET600ReadWaiting(i) = False
                    ET600ReadTimeout(i) = 0
                    rxBuff(i) = ""

                Next

                ' Start meter communication for PollType 8
                EnableMeterTimer()

                ' Reference meter is not required for CT setup write
                RefTimer.Enabled = False

                ' Restart process timer so PollType 8 completion is checked
                SNGTimer.Interval = 2000
                SNGTimer.Enabled = True

            End If

        End If

    End Sub

    Private Sub UpdateErrData()

        Dim strConnection As String = ""
        Dim ELNetCon As SqlConnection = New SqlConnection(strConnection)
        Dim SQLStr As String = ""
        Dim mycommand As SqlCommand
        'Dim CurrentSlNo As String = ""

        ' Try
        strConnection = "Data Source=192.168.5.200; Initial Catalog=Elmeasure EPS;Integrated Security=False;User ID=sa; Password=mEA9269mK*$47@bAG"
        ELNetCon = New SqlConnection(strConnection)
        ELNetCon.Open()


        For i = 1 To NoOfMUT
            If MeterReadFail(i) < 10 Then

                'CurrentSlNo = Trim(Str(MeterSACardNo(i)))

                For k = 1 To 7

                    SQLStr = "INSERT INTO EPS_Verification " &
                            "(CompanyID,CalNo,SlNo,Date,TestNo,Test,Ref_Dv," &
                                "W_Tot,Wr,Wy,Wb," &
                                "VAR_Tot,VARr,VARy,VARb," &
                                "PF,PFr,PFy,PFb," &
                                "VA_Tot,VA_r,VA_y,VA_b," &
                                "VLL,Vry,Vyb,Vbr," &
                                "VLN,Vr,Vy,Vb,Amps,Ar,Ay,Ab,Hz) VALUES (" &
                                "7," & Trim(Str(Calno)) & "," & Trim(Str(MeterSACardNo(i))) & ",'" & Now.ToString("yyyy-MM-dd HH:mm") & "'," &
                                 Trim(Str(k - 1)) & ",'" & Trim(Str(VerVolt(k))) & "','E'," &
                                 "0.102,0.102,0.102,0.102," &
                                 "0.102,0.102,0.102,0.102," &
                            Trim(Str(VerErrData(i, 14, k))) & "," & Trim(Str(VerErrData(i, 7, k))) & "," & Trim(Str(VerErrData(i, 8, k))) & "," & Trim(Str(VerErrData(i, 9, k))) & "," &
                            "0.102,0.102,0.102,0.102," &
                            "0.102,0.102,0.102,0.102," &
                            Trim(Str(VerErrData(i, 21, k))) & "," & Trim(Str(VerErrData(i, 1, k))) & "," & Trim(Str(VerErrData(i, 2, k))) & "," & Trim(Str(VerErrData(i, 3, k))) & "," &
                            Trim(Str(VerErrData(i, 25, k))) & "," & Trim(Str(VerErrData(i, 4, k))) & "," & Trim(Str(VerErrData(i, 5, k))) & "," & Trim(Str(VerErrData(i, 6, k))) & "," & Trim(Str(VerErrData(i, 29, k))) & ")"


                    mycommand = New SqlCommand(SQLStr, ELNetCon)
                    'mycommand.ExecuteNonQuery()
                    Try
                        mycommand.ExecuteNonQuery()

                    Catch ex As Exception

                        MsgBox("Verification data not saved." & vbCrLf & "Serial Number : " & MeterSACardNo(i) & vbCrLf & "Test No : " & k & vbCrLf & ex.Message, MsgBoxStyle.Critical, "Save Failed")

                        ErrorEvents("Verification save failed. SlNo=" & MeterSACardNo(i) & " Test=" & k & " Error=" & ex.Message)

                        If ELNetCon.State = ConnectionState.Open Then
                            ELNetCon.Close()
                        End If

                        Exit Sub

                    End Try

                Next

            End If
        Next

        If ELNetCon.State = ConnectionState.Open Then
            ELNetCon.Close()
        End If


        'Catch ex As Exception
        '    If ELNetCon.State = ConnectionState.Closed Then
        '        MsgBox("Unable to conecct Elmeasure Server!", vbCritical)
        '        ErrorEvents("UpdateErrData : Unable to conecct Elmeasure Server! Cal No=" + Str(Calno))
        '    End If
        'End Try

    End Sub

    Private Sub FillDataGrid(ByVal Metno As Byte)

        Select Case Metno
            Case 1
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(1)
            Case 2
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(2)
            Case 3
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(3)
            Case 4
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(4)
            Case 5
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(5)
            Case 6
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(6)

        End Select

        If Metno = 0 Then
            If ProcessType = 1 Then
                DataGridView1.Item(1, Metno).Value = MeterData(Metno, CalStep, 21)
                DataGridView1.Item(2, Metno).Value = MeterData(Metno, CalStep, 25)
                DataGridView1.Item(3, Metno).Value = MeterData(Metno, CalStep, 9)
                DataGridView1.Item(4, Metno).Value = MeterData(Metno, CalStep, 10)
                DataGridView1.Item(5, Metno).Value = MeterData(Metno, CalStep, 11)
                DataGridView1.Item(6, Metno).Value = MeterData(Metno, CalStep, 12)
                DataGridView1.Item(7, Metno).Value = MeterData(Metno, CalStep, 1)
                DataGridView1.Item(8, Metno).Value = MeterData(Metno, CalStep, 2)
                DataGridView1.Item(9, Metno).Value = MeterData(Metno, CalStep, 3)
                DataGridView1.Item(10, Metno).Value = MeterData(Metno, CalStep, 4)
                DataGridView1.Item(11, Metno).Value = MeterData(Metno, CalStep, 29)
            Else
                DataGridView1.Item(1, Metno).Value = MeterData(Metno, VerfStep, 21)
                DataGridView1.Item(2, Metno).Value = MeterData(Metno, VerfStep, 25)
                DataGridView1.Item(3, Metno).Value = MeterData(Metno, VerfStep, 9)
                DataGridView1.Item(4, Metno).Value = MeterData(Metno, VerfStep, 10)
                DataGridView1.Item(5, Metno).Value = MeterData(Metno, VerfStep, 11)
                DataGridView1.Item(6, Metno).Value = MeterData(Metno, VerfStep, 12)
                DataGridView1.Item(7, Metno).Value = MeterData(Metno, VerfStep, 1)
                DataGridView1.Item(8, Metno).Value = MeterData(Metno, VerfStep, 2)
                DataGridView1.Item(9, Metno).Value = MeterData(Metno, VerfStep, 3)
                DataGridView1.Item(10, Metno).Value = MeterData(Metno, VerfStep, 4)
                DataGridView1.Item(11, Metno).Value = MeterData(Metno, VerfStep, 29)
            End If
        Else
            If ProcessType = 1 Then
                DataGridView1.Item(1, Metno).Value = MeterData(Metno, CalStep, 21)
                DataGridView1.Item(2, Metno).Value = MeterData(Metno, CalStep, 25)
                DataGridView1.Item(3, Metno).Value = MeterData(Metno, CalStep, 14)
                DataGridView1.Item(4, Metno).Value = MeterData(Metno, CalStep, 7)
                DataGridView1.Item(5, Metno).Value = MeterData(Metno, CalStep, 8)
                DataGridView1.Item(6, Metno).Value = MeterData(Metno, CalStep, 9)
                DataGridView1.Item(7, Metno).Value = MeterData(Metno, CalStep, 10)
                DataGridView1.Item(8, Metno).Value = MeterData(Metno, CalStep, 11)
                DataGridView1.Item(9, Metno).Value = MeterData(Metno, CalStep, 12)
                DataGridView1.Item(10, Metno).Value = MeterData(Metno, CalStep, 13)
                DataGridView1.Item(11, Metno).Value = MeterData(Metno, CalStep, 29)
            Else
                DataGridView1.Item(1, Metno).Value = MeterData(Metno, VerfStep, 21)
                DataGridView1.Item(2, Metno).Value = MeterData(Metno, VerfStep, 25)
                DataGridView1.Item(3, Metno).Value = MeterData(Metno, VerfStep, 14)
                DataGridView1.Item(4, Metno).Value = MeterData(Metno, VerfStep, 7)
                DataGridView1.Item(5, Metno).Value = MeterData(Metno, VerfStep, 8)
                DataGridView1.Item(6, Metno).Value = MeterData(Metno, VerfStep, 9)
                DataGridView1.Item(7, Metno).Value = MeterData(Metno, VerfStep, 10)
                DataGridView1.Item(8, Metno).Value = MeterData(Metno, VerfStep, 11)
                DataGridView1.Item(9, Metno).Value = MeterData(Metno, VerfStep, 12)
                DataGridView1.Item(10, Metno).Value = MeterData(Metno, VerfStep, 13)
                DataGridView1.Item(11, Metno).Value = MeterData(Metno, VerfStep, 29)
            End If
        End If


    End Sub

    Private Sub FillDataGridBlank(ByVal Metno As Byte)


        If Metno >= DataGridView1.Rows.Count Then Exit Sub

        For i As Integer = 0 To DataGridView1.Columns.Count - 1
            DataGridView1.Item(i, Metno).Value = ""
        Next


    End Sub


    Private Sub FillErrDataGrid(ByVal Metno As Byte)

        Select Case Metno
            Case 1
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(1)
            Case 2
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(2)
            Case 3
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(3)
            Case 4
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(4)
            Case 5
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(5)
            Case 6
                DataGridView1.Item(0, Metno).Value = MeterSACardNo(6)

        End Select

        If MeterReadFail(Metno) < 10 Then
            If AccClass(Metno) = 4 Then
                DataGridView1.Rows(Metno).DefaultCellStyle.BackColor = Color.Red
                DataGridView1.Item(1, Metno).Value = "Fail"
            ElseIf AccClass(Metno) = 1 Then
                DataGridView1.Rows(Metno).DefaultCellStyle.BackColor = Color.White
                DataGridView1.Item(1, Metno).Value = "Pass 1.0"
            ElseIf AccClass(Metno) = 2 Then
                DataGridView1.Rows(Metno).DefaultCellStyle.BackColor = Color.Orange
                DataGridView1.Item(1, Metno).Value = "Pass 0.5"
            ElseIf AccClass(Metno) = 3 Then
                DataGridView1.Rows(Metno).DefaultCellStyle.BackColor = Color.Green
                DataGridView1.Item(1, Metno).Value = "Pass 0.2"
            End If
        Else
            DataGridView1.Rows(Metno).DefaultCellStyle.BackColor = Color.Red
            DataGridView1.Item(1, Metno).Value = "Fail"
        End If

    End Sub

    Private Sub UpdateAccuracy()

        Dim strConnection As String = ""
        Dim ELNetCon As SqlConnection = New SqlConnection(strConnection)
        Dim SQLStr As String = ""
        Dim mycommand As SqlCommand

        'Try
        strConnection = "Data Source=192.168.5.200; Initial Catalog=Elmeasure;Integrated Security=False;User ID=sa; Password=mEA9269mK*$47@bAG"
        ELNetCon = New SqlConnection(strConnection)
        ELNetCon.Open()

        For i = 1 To NoOfMUT
            If MeterReadFail(i) < 10 Then
                SQLStr = "update EPS_CalConstants set Obt_Class = " + Str(AccClass(i)) + ", CalBy=1, CalDate ='" + Now.ToString("yyyy-MM-dd HH:mm") + "', Calno= " + Str(Calno) + " where slno=" + Str(MeterSACardNo(i))
                mycommand = New SqlCommand(SQLStr, ELNetCon)
                Debug.Print(SQLStr)
                'MsgBox(SQLStr)
                mycommand.ExecuteNonQuery()
            End If
        Next

        'Catch ex As Exception
        '    If ELNetCon.State = ConnectionState.Closed Then
        '        MsgBox("UpdateAccuracy : Unable to conecct Elmeasure Server! Cal No=" + Str(Calno), vbCritical)
        '        ErrorEvents("UpdateAccuracy : Unable to conecct Elmeasure Server! Cal No=" + Str(Calno))
        '    End If
        'End Try

    End Sub

    Private Sub DisableTimer()

        RefTimer.Enabled = False
        Timer1.Enabled = False
        Timer2.Enabled = False
        Timer3.Enabled = False
        Timer4.Enabled = False
        Timer5.Enabled = False
        Timer6.Enabled = False


    End Sub

    Private Sub EnableTimer()

        RefTimer.Enabled = True
        If NoOfMUT >= 1 Then
            Timer1.Enabled = True
        End If
        If NoOfMUT >= 2 Then
            Timer2.Enabled = True
        End If
        If NoOfMUT >= 3 Then
            Timer3.Enabled = True
        End If
        If NoOfMUT >= 4 Then
            Timer4.Enabled = True
        End If
        If NoOfMUT >= 5 Then
            Timer5.Enabled = True
        End If
        If NoOfMUT >= 6 Then
            Timer6.Enabled = True
        End If

    End Sub

    Private Sub EnableMeterTimer()

        RefTimer.Enabled = True
        If NoOfMUT >= 1 And MeterSACardNo(1) > 0 Then
            Timer1.Enabled = True
        End If
        If NoOfMUT >= 2 And MeterSACardNo(2) > 0 Then
            Timer2.Enabled = True
        End If
        If NoOfMUT >= 3 And MeterSACardNo(3) > 0 Then
            Timer3.Enabled = True
        End If
        If NoOfMUT >= 4 And MeterSACardNo(4) > 0 Then
            Timer4.Enabled = True
        End If
        If NoOfMUT >= 5 And MeterSACardNo(5) > 0 Then
            Timer5.Enabled = True
        End If
        If NoOfMUT >= 6 And MeterSACardNo(6) > 0 Then
            Timer6.Enabled = True
        End If

    End Sub

    Private Sub FindAVeCalData()
        Dim calstr As String

        For i = 0 To NoOfMUT
            If MeterReadFail(i) < 10 Then
                For k = 1 To Sample(i)
                    calstr = "Cal: Meter" + Str(i) + "," + Str(k)
                    For j = 1 To 29
                        AveCalData(i, CalStep, j) = AveCalData(i, CalStep, j) + CalMeterData(i, j, k)
                        calstr = calstr + "," + Str(CalMeterData(i, j, k))
                    Next
                    ErrorEvents3(calstr)
                Next
                If Sample(i) > 0 Then
                    calstr = "Step:" + Str(CalStep) + ",Ave"
                    For j = 1 To 29
                        AveCalData(i, CalStep, j) = AveCalData(i, CalStep, j) / Sample(i)
                        calstr = calstr + "," + Str(AveCalData(i, CalStep, j))
                    Next
                    ErrorEvents3(calstr)
                End If
            End If
        Next

    End Sub

    Private Sub FindAVeVerData()
        Dim AveVerData(6, 29)
        Dim calstr As String

        For i = 0 To 6
            ErrorEvents("Meter=" & i &
            " Sample=" & Sample(i) &
            " MeterReadFail=" & MeterReadFail(i))
            If MeterReadFail(i) < 10 Then
                For k = 1 To Sample(i)
                    calstr = "Ver:" + Str(i) + "," + Str(k)
                    For j = 1 To 29
                        AveVerData(i, j) = AveVerData(i, j) + VerMeterData(i, j, k)
                        calstr = calstr + "," + Str(VerMeterData(i, j, k))
                    Next
                    ErrorEvents3(calstr)
                Next
                calstr = Str(i) + ",k"
                'For j = 1 To 29
                '    If j = 1 Or j = 9 Or j = 21 Or j = 25 Then
                '        j = j
                '    End If
                '    AveVerData(i, j) = AveVerData(i, j) / Sample(i)
                '    AveVerDataSteps(i, j, VerfStep) = AveVerData(i, j)
                '    calstr = calstr + "," + Str(AveVerData(i, j))
                'Next

                If Sample(i) = 0 Then
                    ErrorEvents("WARNING : Meter " & i & " Sample Count = 0")
                End If
                For j = 1 To 29

                    If Sample(i) > 0 Then
                        AveVerData(i, j) = AveVerData(i, j) / Sample(i)
                    Else
                        AveVerData(i, j) = 0
                    End If

                    AveVerDataSteps(i, j, VerfStep) = AveVerData(i, j)
                    calstr = calstr + "," + Str(AveVerData(i, j))

                Next
                ErrorEvents3(calstr)
            End If
        Next

        'For i = 1 To NoOfMUT
        '    If MeterReadFail(i) < 10 Then
        '        AveVerDataSteps(i, 1, VerfStep) = AveVerDataSteps(i, 10, VerfStep)
        '        AveVerDataSteps(i, 9, VerfStep) = AveVerDataSteps(i, 14, VerfStep)
        '    End If
        'Next

    End Sub

    '=========================================================
    ' SAFE PERCENT ERROR
    '=========================================================
    Private Function CalcPercentError(ByVal DutValue As Double, ByVal RefValue As Double) As Single

        If RefValue = 0 Then
            Return 2
        End If

        Dim ErrValue As Double

        ErrValue = ((DutValue * 100) / RefValue) - 100


        If Double.IsNaN(ErrValue) OrElse Double.IsInfinity(ErrValue) Then

            Return 2

        End If

        Return CSng(ErrValue)

    End Function


    'Private Sub FindCalConstant()

    '    Dim RefPA As Single
    '    Dim DutPA As Single
    '    Dim PAErr As Single
    '    Dim Wcal As Single
    '    Dim LogStr As String

    '    For j = 1 To NoOfMUT
    '        If MeterReadFail(j) < 10 Then
    '            For i = 1 To 31
    '                Select Case i
    '                    Case 1
    '                        CalConstData(j, i) = Math.Round(AveCalData(0, 1, 25), 5)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 25))
    '                    Case 2
    '                        CalConstData(j, i) = Math.Round(AveCalData(0, 2, 25), 5)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 2, 25))
    '                    Case 3
    '                        CalConstData(j, i) = Math.Round(AveCalData(0, 3, 25), 5)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 3, 25))
    '                    Case 4
    '                        CalConstData(j, i) = Math.Round(AveCalData(0, 4, 25), 5)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 4, 25))
    '                    Case 5
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 1, 21) / AveCalData(j, 1, 1)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 21)) + "," + Str(AveCalData(j, 1, 1)) + "," + Str(CalConstData(j, i))
    '                    Case 6
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 1, 21) / AveCalData(j, 1, 2)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 21)) + "," + Str(AveCalData(j, 1, 2)) + "," + Str(CalConstData(j, i))
    '                    Case 7
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 1, 21) / AveCalData(j, 1, 3)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 21)) + "," + Str(AveCalData(j, 1, 3)) + "," + Str(CalConstData(j, i))
    '                    Case 8
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 1, 25) / AveCalData(j, 1, 4)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 25)) + "," + Str(AveCalData(j, 1, 4)) + "," + Str(CalConstData(j, i))
    '                    Case 9
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 1, 25) / AveCalData(j, 1, 5)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 25)) + "," + Str(AveCalData(j, 1, 5)) + "," + Str(CalConstData(j, i))
    '                    Case 10
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 1, 25) / AveCalData(j, 1, 6)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 25)) + "," + Str(AveCalData(j, 1, 6)) + "," + Str(CalConstData(j, i))
    '                    Case 11
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 2, 25) / AveCalData(j, 2, 4)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 2, 25)) + "," + Str(AveCalData(j, 2, 4)) + "," + Str(CalConstData(j, i))
    '                    Case 12
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 2, 25) / AveCalData(j, 2, 5)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 2, 25)) + "," + Str(AveCalData(j, 2, 5)) + "," + Str(CalConstData(j, i))
    '                    Case 13
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 2, 25) / AveCalData(j, 2, 6)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 2, 25)) + "," + Str(AveCalData(j, 2, 6)) + "," + Str(CalConstData(j, i))
    '                    Case 14
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 3, 25) / AveCalData(j, 3, 4)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 3, 25)) + "," + Str(AveCalData(j, 3, 4)) + "," + Str(CalConstData(j, i))
    '                    Case 15
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 3, 25) / AveCalData(j, 3, 5)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 3, 25)) + "," + Str(AveCalData(j, 3, 5)) + "," + Str(CalConstData(j, i))
    '                    Case 16
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 3, 25) / AveCalData(j, 3, 6)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 3, 25)) + "," + Str(AveCalData(j, 3, 6)) + "," + Str(CalConstData(j, i))
    '                    Case 17
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 4, 25) / AveCalData(j, 4, 4)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 4, 25)) + "," + Str(AveCalData(j, 4, 4)) + "," + Str(CalConstData(j, i))
    '                    Case 18
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 4, 25) / AveCalData(j, 4, 5)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 4, 25)) + "," + Str(AveCalData(j, 4, 5)) + "," + Str(CalConstData(j, i))
    '                    Case 19
    '                        CalConstData(j, i) = Math.Round(((AveCalData(0, 4, 25) / AveCalData(j, 4, 6)) - 1) * 32767 * 16)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 4, 25)) + "," + Str(AveCalData(j, 4, 6)) + "," + Str(CalConstData(j, i))
    '                    Case 20
    '                        RefPA = AveCalData(0, 1, 9)
    '                        DutPA = AveCalData(j, 1, 7)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 9)) + "," + Str(AveCalData(j, 1, 7)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 1, 9)) + "," + Str(AveCalData(j, 1, 7)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 21
    '                        RefPA = AveCalData(0, 1, 9)
    '                        DutPA = AveCalData(j, 1, 8)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 9)) + "," + Str(AveCalData(j, 1, 8)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 1, 9)) + "," + Str(AveCalData(j, 1, 8)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 22
    '                        RefPA = AveCalData(0, 1, 9)
    '                        DutPA = AveCalData(j, 1, 9)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 1, 9)) + "," + Str(AveCalData(j, 1, 9)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 1, 9)) + "," + Str(AveCalData(j, 1, 9)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 23
    '                        RefPA = AveCalData(0, 2, 9)
    '                        DutPA = AveCalData(j, 2, 7)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 2, 9)) + "," + Str(AveCalData(j, 2, 7)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 2, 9)) + "," + Str(AveCalData(j, 2, 7)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 24
    '                        RefPA = AveCalData(0, 2, 9)
    '                        DutPA = AveCalData(j, 2, 8)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 2, 9)) + "," + Str(AveCalData(j, 2, 8)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 2, 9)) + "," + Str(AveCalData(j, 2, 8)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 25
    '                        RefPA = AveCalData(0, 2, 9)
    '                        DutPA = AveCalData(j, 2, 9)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 2, 9)) + "," + Str(AveCalData(j, 2, 9)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 2, 9)) + "," + Str(AveCalData(j, 2, 9)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 26
    '                        RefPA = AveCalData(0, 3, 9)
    '                        DutPA = AveCalData(j, 3, 7)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 3, 9)) + "," + Str(AveCalData(j, 3, 7)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 3, 9)) + "," + Str(AveCalData(j, 3, 7)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 27
    '                        RefPA = AveCalData(0, 3, 9)
    '                        DutPA = AveCalData(j, 3, 8)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 3, 9)) + "," + Str(AveCalData(j, 3, 8)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 3, 9)) + "," + Str(AveCalData(j, 3, 8)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 28
    '                        RefPA = AveCalData(0, 3, 9)
    '                        DutPA = AveCalData(j, 3, 9)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 3, 9)) + "," + Str(AveCalData(j, 3, 9)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 3, 9)) + "," + Str(AveCalData(j, 3, 9)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 29
    '                        RefPA = FindPA(AveCalData(0, 4, 9))
    '                        DutPA = FindPA(AveCalData(j, 4, 7))
    '                        PAErr = RefPA - DutPA
    '                        PAErr = PAErr * ((4 * Math.Atan(1)) / 180)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 4, 9)) + "," + Str(AveCalData(j, 4, 7)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 4, 9)) + "," + Str(AveCalData(j, 4, 7)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 30
    '                        RefPA = AveCalData(0, 4, 9)
    '                        DutPA = AveCalData(j, 4, 8)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 4, 9)) + "," + Str(AveCalData(j, 4, 8)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 4, 9)) + "," + Str(AveCalData(j, 4, 8)) + "," + Str(CalConstData(j, i + 12))
    '                    Case 31
    '                        RefPA = AveCalData(0, 4, 9)
    '                        DutPA = AveCalData(j, 4, 9)
    '                        PAErr = Math.Acos(RefPA) - Math.Acos(DutPA)
    '                        Wcal = Math.Cos(PAErr) * 32767 * 16
    '                        If Wcal >= 0 Then
    '                            Wcal = Wcal - 491520
    '                        Else
    '                            Wcal = Wcal + 491520
    '                        End If
    '                        CalConstData(j, i) = Math.Round(Wcal)
    '                        CalConstData(j, i + 12) = Math.Round(Math.Sin(PAErr) * 32767)
    '                        LogStr = Str(j) + "," + Str(i) + "," + Str(AveCalData(0, 4, 9)) + "," + Str(AveCalData(j, 4, 9)) + "," + Str(CalConstData(j, i))
    '                        LogStr = LogStr + vbCrLf
    '                        ErrorEvents(LogStr)
    '                        LogStr = Str(j) + "," + Str(i + 12) + "," + Str(AveCalData(0, 4, 9)) + "," + Str(AveCalData(j, 4, 9)) + "," + Str(CalConstData(j, i + 12))
    '                End Select
    '                LogStr = LogStr + vbCrLf
    '                ErrorEvents(LogStr)
    '            Next
    '        End If
    '    Next

    '    UpdateCalConstants()

    '    Dim TString As String

    '    For j = 1 To NoOfMUT
    '        If MeterReadFail(j) <= 10 Then
    '            CalConstantStr(j) = "0110AA551B58000204"
    '            For i = 1 To 4
    '                TString = dec_to_float(CalConstData(j, i))
    '                CalConstantStr(j) = CalConstantStr(j) + Mid(TString, 7, 2) + Mid(TString, 5, 2) + Mid(TString, 3, 2) + Mid(TString, 1, 2)
    '            Next
    '            Dim TVal As Short
    '            For i = 5 To 31
    '                If Math.Abs(CalConstData(j, i)) > 32767 Then
    '                    If CalConstData(j, i) > 32767 Then
    '                        If i <= 19 Then
    '                            CalConstData(j, i) = 32767
    '                        Else
    '                            CalConstData(j, i) = 32752
    '                        End If
    '                    Else
    '                        If i <= 19 Then
    '                            CalConstData(j, i) = -32767
    '                        Else
    '                            CalConstData(j, i) = -32752
    '                        End If
    '                    End If
    '                    TVal = CalConstData(j, i)
    '                Else
    '                    TVal = CalConstData(j, i)
    '                End If
    '                TString = Hex(TVal)
    '                If Len(TString) = 1 Then
    '                    TString = "000" + TString
    '                ElseIf Len(TString) = 2 Then
    '                    TString = "00" + TString
    '                ElseIf Len(TString) = 3 Then
    '                    TString = "0" + TString
    '                End If
    '                CalConstantStr(j) = CalConstantStr(j) + Mid(TString, 3, 2) + Mid(TString, 1, 2)
    '            Next i
    '            For i = 20 To 31
    '                If Math.Abs(CalConstData(j, i)) > 32767 Then
    '                    CalConstData(j, i + 12) = 0
    '                End If
    '                TString = Hex(CalConstData(j, i + 12))
    '                If Len(TString) = 1 Then
    '                    TString = "000" + TString
    '                ElseIf Len(TString) = 2 Then
    '                    TString = "00" + TString
    '                ElseIf Len(TString) = 3 Then
    '                    TString = "0" + TString
    '                End If
    '                CalConstantStr(j) = CalConstantStr(j) + Mid(TString, 3, 2) + Mid(TString, 1, 2)
    '            Next i
    '            ErrorEvents4(CalConstantStr(j))
    '        End If
    '    Next

    'End Sub

    Private Sub FindCalConstant()

        Dim MeterNo As Integer
        Dim Level As Integer
        Dim Channel As Integer

        Dim DutCurrentIndex(6) As Integer
        Dim DutPFIndex(6) As Integer

        Dim RefCurrent As Double
        Dim DutCurrent As Double

        Dim RefPF As Double
        Dim DutPF As Double

        Dim PAErr As Double

        Dim CurrentCalValue As Double
        Dim VoltageCalValue As Double
        Dim WattCalValue As Double
        Dim VarCalValue As Double

        Dim Packet As String


        '=========================================================
        ' ET600 METERDATA MAPPING
        '
        ' Current:
        ' CH1 = 4
        ' CH2 = 5
        ' CH3 = 6
        ' CH4 = 15
        ' CH5 = 16
        ' CH6 = 17
        '
        ' PF:
        ' CH1 = 7
        ' CH2 = 8
        ' CH3 = 9
        ' CH4 = 18
        ' CH5 = 19
        ' CH6 = 20
        '=========================================================

        DutCurrentIndex(1) = 4
        DutCurrentIndex(2) = 5
        DutCurrentIndex(3) = 6
        DutCurrentIndex(4) = 15
        DutCurrentIndex(5) = 16
        DutCurrentIndex(6) = 17

        DutPFIndex(1) = 7
        DutPFIndex(2) = 8
        DutPFIndex(3) = 9
        DutPFIndex(4) = 18
        DutPFIndex(5) = 19
        DutPFIndex(6) = 20


        '=========================================================
        ' STEP 1
        ' STORE THE 5 ACTUAL REFERENCE CURRENTS
        '
        ' Excel addresses:
        ' 7001 = level 1
        ' 7003 = level 2
        ' 7005 = level 3
        ' 7007 = level 4
        ' 7009 = level 5
        '=========================================================

        For Level = 1 To 5

            ET600RefCurrent(Level) = CSng(AveCalData(0, Level, 25))

            ErrorEvents("ET600 REF CURRENT Level=" & Level & " Value=" & ET600RefCurrent(Level))

        Next


        '=========================================================
        ' CALCULATE CONSTANTS FOR EACH MUT
        '=========================================================

        For MeterNo = 1 To NoOfMUT

            If MeterReadFail(MeterNo) < 10 Then


                '=================================================
                ' INITIALISE ALL 12 CHANNEL SLOTS
                '
                ' ET600 only uses CH1-CH6.
                '
                ' CH7-CH12 remain workbook defaults:
                '
                ' Current = 0
                ' Watt    = F07F = 32752
                ' VAR     = 0
                '=================================================

                For Level = 1 To 5

                    For Channel = 1 To 12

                        ET600CurrentCal(MeterNo, Level, Channel) = 0

                        ET600WattCal(MeterNo, Level, Channel) = CUShort(32752)

                        ET600VarCal(MeterNo, Level, Channel) = 0

                    Next

                Next


                '=================================================
                ' VOLTAGE CAL CONSTANT
                '
                ' 7011 = Voltage R
                ' 7013 = Voltage Y
                ' 7015 = Voltage B
                '
                ' Preserve old EPS calculation.
                ' Voltage calibration is taken from level 1.
                '=================================================

                If AveCalData(MeterNo, 1, 1) <> 0 Then

                    VoltageCalValue = ((AveCalData(0, 1, 21) / AveCalData(MeterNo, 1, 1)) - 1) * 32767 * 16

                    ET600VoltageCal(MeterNo, 1) = ClampInt16(VoltageCalValue)


                Else

                    ET600VoltageCal(MeterNo, 1) = 0

                End If


                If AveCalData(MeterNo, 1, 2) <> 0 Then

                    VoltageCalValue = ((AveCalData(0, 1, 21) / AveCalData(MeterNo, 1, 2)) - 1) * 32767 * 16

                    ET600VoltageCal(MeterNo, 2) = ClampInt16(VoltageCalValue)

                Else

                    ET600VoltageCal(MeterNo, 2) = 0

                End If

                If AveCalData(MeterNo, 1, 3) <> 0 Then

                    VoltageCalValue = ((AveCalData(0, 1, 21) / AveCalData(MeterNo, 1, 3)) - 1) * 32767 * 16

                    ET600VoltageCal(MeterNo, 3) = ClampInt16(VoltageCalValue)

                Else

                    ET600VoltageCal(MeterNo, 3) = 0

                End If


                ErrorEvents("ET600 VOLT CC Meter=" & MeterNo & " R=" & ET600VoltageCal(MeterNo, 1) & " Y=" & ET600VoltageCal(MeterNo, 2) & " B=" & ET600VoltageCal(MeterNo, 3))

                '=================================================
                ' CURRENT + WATT + VAR
                '
                ' 5 CALIBRATION LEVELS
                ' 6 ET600 CHANNELS
                '=================================================

                For Level = 1 To 5

                    RefCurrent = AveCalData(0, Level, 25)

                    RefPF = ClampPF(AveCalData(0, Level, 9))

                    For Channel = 1 To 6
                        '=========================================
                        ' CURRENT CAL CONSTANT
                        '=========================================

                        DutCurrent = AveCalData(MeterNo, Level, DutCurrentIndex(Channel))

                        If DutCurrent <> 0 Then

                            CurrentCalValue = ((RefCurrent / DutCurrent) - 1) * 32767 * 16

                            ET600CurrentCal(MeterNo, Level, Channel) = ClampInt16(CurrentCalValue)

                        Else

                            ET600CurrentCal(MeterNo, Level, Channel) = 0

                            ErrorEvents("WARNING ET600 Current=0 Meter=" & MeterNo & " Level=" & Level & " Channel=" & Channel)

                        End If

                        '=========================================
                        ' WATT / VAR CAL CONSTANT
                        '
                        ' Preserve the existing software's phase
                        ' correction principle:
                        '
                        ' PAErr = acos(reference PF)
                        '       - acos(DUT PF)
                        '
                        ' Watt default at zero phase error:
                        ' 32752 = &H7FF0 = bytes F0 7F
                        '
                        ' VAR default at zero phase error:
                        ' 0 = 00 00
                        '=========================================

                        DutPF = ClampPF(AveCalData(MeterNo, Level, DutPFIndex(Channel)))

                        PAErr = Math.Acos(RefPF) - Math.Acos(DutPF)

                        WattCalValue = Math.Cos(PAErr) * 32767 * 16

                        If WattCalValue >= 0 Then

                            WattCalValue = WattCalValue - 491520

                        Else

                            WattCalValue = WattCalValue + 491520

                        End If

                        ET600WattCal(MeterNo, Level, Channel) = ClampUInt16(WattCalValue)

                        VarCalValue = Math.Sin(PAErr) * 32767

                        ET600VarCal(MeterNo, Level, Channel) = ClampInt16(VarCalValue)

                        ErrorEvents("ET600 CC" & " M=" & MeterNo & " L=" & Level & " CH=" & Channel & " RefA=" & RefCurrent & " DutA=" & DutCurrent & " ICal=" & ET600CurrentCal(MeterNo, Level, Channel) & " RefPF=" & RefPF & " DutPF=" & DutPF & " Watt=" & ET600WattCal(MeterNo, Level, Channel) & " Var=" & ET600VarCal(MeterNo, Level, Channel))

                    Next

                Next

                '=================================================
                ' BUILD COMPLETE ET600 DCC WRITE PACKET
                '
                ' HEADER = 9 bytes before payload
                '
                ' 5 x Float             = 20 bytes
                ' 3 x INT16             = 6 bytes
                ' 60 x INT16 current    = 120 bytes
                ' 60 x UINT16 Watt      = 120 bytes
                ' 60 x INT16 VAR        = 120 bytes
                '
                ' Total pre-CRC = 395 bytes
                '=================================================

                Packet = "0110AA551B58000204"
                '=================================================
                ' REFERENCE CURRENT - 5 FLOATS
                '=================================================

                For Level = 1 To 5

                    Packet &= FloatLEHex(ET600RefCurrent(Level))

                Next

                '=================================================
                ' VOLTAGE R/Y/B - 3 INT16
                '=================================================

                For Channel = 1 To 3

                    Packet &= Int16LEHex(ET600VoltageCal(MeterNo, Channel))

                Next

                '=================================================
                ' CURRENT CAL CONSTANTS
                '
                ' Ordering required by workbook:
                '
                ' Level1 CH1..CH12
                ' Level2 CH1..CH12
                ' ...
                ' Level5 CH1..CH12
                '=================================================

                For Level = 1 To 5

                    For Channel = 1 To 12

                        Packet &= Int16LEHex(ET600CurrentCal(MeterNo, Level, Channel))

                    Next

                Next

                '=================================================
                ' WATT CAL CONSTANTS
                '
                ' UINT16 LITTLE ENDIAN
                '=================================================

                For Level = 1 To 5

                    For Channel = 1 To 12

                        Packet &= UInt16LEHex(ET600WattCal(MeterNo, Level, Channel))

                    Next

                Next

                '=================================================
                ' VAR CAL CONSTANTS
                '
                ' INT16 LITTLE ENDIAN
                '=================================================

                For Level = 1 To 5

                    For Channel = 1 To 12

                        Packet &= Int16LEHex(ET600VarCal(MeterNo, Level, Channel))

                    Next

                Next

                CalConstantStr(MeterNo) = Packet

                '=================================================
                ' PACKET LENGTH CHECK
                '
                ' Must be exactly 395 bytes BEFORE CRC.
                '=================================================

                If (Len(CalConstantStr(MeterNo)) \ 2) <> 395 Then

                    ErrorEvents("ET600 DCC PACKET LENGTH ERROR Meter=" & MeterNo & " Length=" & (Len(CalConstantStr(MeterNo)) \ 2))

                    MsgBox("ET600 DCC packet length error." & vbCrLf & "Meter : " & MeterNo & vbCrLf & "Length : " & (Len(CalConstantStr(MeterNo)) \ 2) & vbCrLf & "Expected : 395")

                Else

                    ErrorEvents("ET600 DCC PAC KET OK Meter=" & MeterNo & " PreCRC Bytes=395")

                End If

                ErrorEvents4(CalConstantStr(MeterNo))

            End If

        Next


    End Sub


    Function FindPA(ByVal X As Double)
        Dim pi As Double
        Dim acosx As Double
        If X = 1 Then X = 0.999
        pi = 4 * Math.Atan(1)
        acosx = Math.Atan(-X / Math.Sqrt(-X * X + 1)) + 2 * Math.Atan(1)
        acosx = (acosx * 180) / pi
        FindPA = acosx
    End Function

    Private Sub UpdateCalConstants()

        Dim strConnection As String = ""
        Dim ELNetCon As SqlConnection = New SqlConnection(strConnection)
        Dim SQLStr As String = ""
        Dim mycommand As SqlCommand

        'Try
        strConnection = "Data Source=192.168.5.200; Initial Catalog=Elmeasure EPS;Integrated Security=False;User ID=sa; Password=mEA9269mK*$47@bAG"
            ELNetCon = New SqlConnection(strConnection)
            ELNetCon.Open()

            For i = 1 To NoOfMUT
                If MeterReadFail(i) <= 10 Then
                    SQLStr = "insert into  EPS_CalConstants (CompanyId, SlNo, Cal1,Cal2,Cal3,Cal4,Cal5,Cal6,Cal7,Cal8,Cal9,Cal10,Cal11,Cal12,Cal13,Cal14,Cal15,Cal16,Cal17,Cal18,Cal19,Cal20,Cal21,Cal22,Cal23,Cal24,Cal25,Cal26,Cal27,Cal28,Cal29,Cal30,Cal31,Cal32,Cal33,Cal34,Cal35,Cal36,Cal37,Cal38,Cal39,Cal40,Cal41,Cal42,Cal43, Date, UserId) values (7," + Str(MeterSACardNo(i))
                'For k = 1 To 43
                '    SQLStr = SQLStr + "," + Str(Val(CalConstData(i, k)))
                'Next
                For k = 1 To 43

                    Dim CalVal As Double = CalConstData(i, k)

                    If Double.IsInfinity(CalVal) OrElse Double.IsNaN(CalVal) Then

                        ErrorEvents("Invalid CalConst Meter=" & i &
                    " Index=" & k &
                    " Value=" & CalVal)

                        CalVal = 0

                    End If

                    SQLStr &= "," & CalVal.ToString(System.Globalization.CultureInfo.InvariantCulture)

                Next
                SQLStr = SQLStr + ",'" + Now.ToString("yyyy-MM-dd HH:mm") + "',0)"
                    mycommand = New SqlCommand(SQLStr, ELNetCon)
                    ErrorEvents(SQLStr)
                    mycommand.ExecuteNonQuery()
                End If
            Next

        'Catch ex As Exception
        '    If ELNetCon.State = ConnectionState.Closed Then
        '        MsgBox("Unable to conecct Elmeasure Server!", vbCritical)
        '        ErrorEvents("UpdateCalConstants : Unable to conecct Elmeasure Server! Cal No=" + Str(Calno))
        '    End If
        'End Try

    End Sub

    Private Sub DeleteCalConstants()

        Dim strConnection As String = ""
        Dim ELNetCon As SqlConnection = New SqlConnection(strConnection)
        Dim SQLStr As String = ""
        Dim mycommand As SqlCommand

        'Try
        strConnection = "Data Source=192.168.5.200; Initial Catalog=Elmeasure EPS;Integrated Security=False;User ID=sa; Password=mEA9269mK*$47@bAG"
        ELNetCon = New SqlConnection(strConnection)
        ELNetCon.Open()


        SQLStr = "delete  EPS_CalConstants where slno in(" + Str(MeterSACardNo(1))
        For i = 2 To NoOfMUT
            SQLStr = SQLStr + "," + Str(MeterSACardNo(i))
        Next
        SQLStr = SQLStr + ")"
        ErrorEvents(SQLStr)
        mycommand = New SqlCommand(SQLStr, ELNetCon)
        mycommand.ExecuteNonQuery()


        SQLStr = "delete  EPS_Verification where slno in(" + Str(MeterSACardNo(1))
        For i = 2 To NoOfMUT
            SQLStr = SQLStr + "," + Str(MeterSACardNo(i))
        Next
        SQLStr = SQLStr + ")"
        mycommand = New SqlCommand(SQLStr, ELNetCon)
        mycommand.ExecuteNonQuery()


        'Catch ex As Exception
        '    If ELNetCon.State = ConnectionState.Closed Then
        '        MsgBox("Unable to conecct Elmeasure Server!", vbCritical)
        '        End
        '    End If
        'End Try

    End Sub

    Private Sub RefTimer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RefTimer.Tick

        endTime = Now

        ' Calculate the difference
        Dim duration As TimeSpan = endTime - startTime
        Label3.Text = duration.ToString("hh\:mm\:ss")

        PollMeters(0)
    End Sub

    Private Sub AxWinsock0_DataArrival(ByVal sender As Object, ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent) Handles AxWinsock0.DataArrival

        Dim i As Integer
        Dim k As UInt16
        Dim l As UInt16
        Dim Data(5) As Byte
        Dim ReceivedCRC As String
        Dim CalculatedCRC As String

        Dim bytes As Integer = AxWinsock0.BytesReceived
        Dim comBuffer As Byte() = New Byte(bytes - 1) {}

        If AxWinsock0.BytesReceived = MeterRxBytes(0) Then
            AxWinsock0.GetData(comBuffer)
            For i = 0 To UBound(comBuffer)
                If Len(Hex(comBuffer(i))) = 1 Then
                    rxBuff(0) = rxBuff(0) + "0" + Hex(comBuffer(i))
                Else
                    rxBuff(0) = rxBuff(0) + Hex(comBuffer(i))
                End If
            Next
        End If

        If Len(rxBuff(0)) = MeterRxBytes(0) * 2 Then
            If Val("&h" + Mid(rxBuff(0), 1, 2)) > 247 Then
                Exit Sub
            End If
            ReceivedCRC = Mid(rxBuff(0), (MeterRxBytes(0) * 2 - 3), 4)
            k = MeterRxBytes(0) - 3
            ReDim Data(k)

            l = 0
            For i = 1 To Len(Mid(rxBuff(0), 1, (MeterRxBytes(0) * 2 - 4))) Step 2
                Data(l) = CByte("&h" + Mid(rxBuff(0), i, 2))
                l = l + 1
            Next
            CalculatedCRC = CRC16(Data)
            PollPkts0(rxBuff(0) + "," + CalculatedCRC + ", M0 PT:" + Str(ProcessType) + ",Sample:" + Str(Sample(0)) + "," + Now.ToString("dd-MM-yy HH:mm"))
            If ReceivedCRC <> CalculatedCRC Then
                Exit Sub
            End If
            MeterReadFail(0) = 0
            Decode_Data(rxBuff(0), 0)
            RefMeterFail = 0
        End If

    End Sub

    'Private Sub AxWinsock1_DataArrival(ByVal sender As Object, ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent) Handles AxWinsock1.DataArrival
    '    Dim i As Integer
    '    Dim k As UInt16
    '    Dim l As UInt16
    '    Dim Data(5) As Byte
    '    Dim ReceivedCRC As String
    '    Dim CalculatedCRC As String

    '    Dim bytes As Integer = AxWinsock1.BytesReceived
    '    Dim comBuffer As Byte() = New Byte(bytes - 1) {}

    '    If AxWinsock1.BytesReceived = MeterRxBytes(1) Then
    '        AxWinsock1.GetData(comBuffer)
    '        For i = 0 To UBound(comBuffer)
    '            If Len(Hex(comBuffer(i))) = 1 Then
    '                rxBuff(1) = rxBuff(1) + "0" + Hex(comBuffer(i))
    '            Else
    '                rxBuff(1) = rxBuff(1) + Hex(comBuffer(i))
    '            End If
    '        Next
    '    End If

    '    If Len(rxBuff(1)) >= MeterRxBytes(1) * 2 Then
    '        If Val("&h" + Mid(rxBuff(1), 1, 2)) > 247 Then
    '            Exit Sub
    '        End If
    '        ReceivedCRC = Mid(rxBuff(1), (MeterRxBytes(1) * 2 - 3), 4)
    '        k = MeterRxBytes(1) - 3
    '        ReDim Data(k)

    '        l = 0
    '        For i = 1 To Len(Mid(rxBuff(1), 1, (MeterRxBytes(1) * 2 - 4))) Step 2
    '            Data(l) = CByte("&h" + Mid(rxBuff(1), i, 2))
    '            l = l + 1
    '        Next
    '        CalculatedCRC = CRC16(Data)
    '        PollPkts1("RX: PT:" + Str(PollType) + ", " + rxBuff(1) + ":" + CalculatedCRC + ",Sample:" + Str(Sample(1)) + "," + Now.ToString("dd-MM-yy HH:mm"))
    '        If ReceivedCRC <> CalculatedCRC Then
    '            Exit Sub
    '        End If
    '        MeterReadFail(1) = 0
    '        If PollType = 0 Then
    '            MeterSACardNo(1) = "&h" & Mid(rxBuff(1), 71, 8)
    '            ListBox1.Items.Add("Meter1 SA Crad Sl.No. Read Completed")
    '            CCWrite(1) = 3
    '        ElseIf PollType = 6 Then
    '            If Sample(1) > 100 Then Exit Sub
    '            Decode_Data(rxBuff(1), 1)
    '        ElseIf PollType = 7 Then
    '            CCWrite(1) = 3
    '            ListBox1.Items.Add("Meter1 CC write Completed")
    '        ElseIf PollType = 1 Then
    '            CCWrite(1) = 3
    '            ListBox1.Items.Add("Meter1 DCC write Completed")
    '        ElseIf PollType = 2 Then
    '            CCWrite(1) = 3
    '            ListBox1.Items.Add("Meter1 DCC Read Completed")
    '        ElseIf PollType = 3 Then
    '            CCWrite(1) = 3
    '            ListBox1.Items.Add("Meter1 DC Offset and Hz default write string Completed")
    '        ElseIf PollType = 4 Then
    '            CCWrite(1) = 3
    '            ListBox1.Items.Add("Meter1 Hz calibration Completed")
    '        ElseIf PollType = 5 Then
    '            CCWrite(1) = 3
    '            ListBox1.Items.Add("Meter1 DC Offset calibration Completed")
    '        End If
    '    End If

    'End Sub

    Private Sub Decode_Data(ByVal tmData As String, ByVal MetNo As Byte)

        Dim i As Integer
        Dim start As Integer
        Dim tinyMasterRawData As String
        Dim binData As String
        Dim AmpVal As Single
        Dim PFVal As Single
        Dim VoltVal As Single
        Dim RefErr, VerErr As Single
        Dim DecodedValue As Single
        Dim SkipET600Sample As Boolean = False

        ' On Error GoTo 100

        If VerfStep = 8 Then Exit Sub

        If PollType < 6 Then GoTo 10



        If MetNo > 0 AndAlso ET600ReadStep(MetNo) <> 2 Then
            GoTo 10                                          ' NEW
        End If

        If InitCount(MetNo) = 200 Then
            InitCount(MetNo) = 12
        End If

        InitCount(MetNo) = InitCount(MetNo) + 1

        If InitCount(MetNo) <= 12 Then

            For i = 0 To 6
                Sample(i) = 0
            Next

            If MetNo = 0 Then
                Exit Sub
            Else
                SkipET600Sample = True                   ' NEW
            End If


            'Exit Sub

        End If


10:     start = 1

        If MetNo = 0 Then
            tmData = Mid(tmData, 7)
            For i = 1 To 29
                tinyMasterRawData = Mid(tmData, start + 4, 4) + Mid(tmData, start, 4)
                binData = LongToBinary(tinyMasterRawData)
                If ProcessType = 1 Or PollType < 6 Then
                    MeterData(MetNo, CalStep, i) = Bin2Float(binData)
                Else
                    MeterData(MetNo, VerfStep, i) = Bin2Float(binData)
                End If
                start = start + 8
            Next
            'If ProcessType <= 1 Then
            '    DataGridView1.Item(1, MetNo).Value = MeterData(MetNo, CalStep, 21)
            '    DataGridView1.Item(2, MetNo).Value = MeterData(MetNo, CalStep, 25)
            '    DataGridView1.Item(11, MetNo).Value = MeterData(MetNo, CalStep, 29)
            'End If

            ' Display all reference meter measurements,
            ' even if voltage/current/PF validation later fails.

            If ProcessType = 1 OrElse PollType < 6 Then

                FillDataGrid(0)

                ErrorEvents(
                    "REF DISPLAY" &
                    " | Step=" & CalStep &
                    " | V=" & MeterData(0, CalStep, 21) &
                    " | I=" & MeterData(0, CalStep, 25) &
                    " | PF=" & MeterData(0, CalStep, 9) &
                    " | WattsT=" & MeterData(0, CalStep, 1) &
                    " | WattsR=" & MeterData(0, CalStep, 2) &
                    " | WattsY=" & MeterData(0, CalStep, 3) &
                    " | WattsB=" & MeterData(0, CalStep, 4)
                )

            ElseIf ProcessType = 2 Then

                FillDataGrid(0)

            End If
            'Else
            '    tmData = Mid(tmData, 11)
            '    For i = 1 To 13
            '        tinyMasterRawData = Mid(tmData, start + 4, 4) + Mid(tmData, start, 4)
            '        binData = LongToBinary(tinyMasterRawData)
            '        If ProcessType = 1 Or PollType < 6 Then
            '            MeterData(MetNo, CalStep, i) = Bin2Float(binData)
            '        Else
            '            MeterData(MetNo, VerfStep, i) = Bin2Float(binData)
            '        End If
            '        start = start + 8
            '    Next
            '    If MetNo > 0 And ProcessType = 1 Then
            '        MeterData(MetNo, CalStep, 21) = (MeterData(MetNo, CalStep, 1) + MeterData(MetNo, CalStep, 2) + MeterData(MetNo, CalStep, 3)) / 3 'V
            '        MeterData(MetNo, CalStep, 25) = (MeterData(MetNo, CalStep, 4) + MeterData(MetNo, CalStep, 5) + MeterData(MetNo, CalStep, 6)) / 3 'A
            '        MeterData(MetNo, CalStep, 14) = (MeterData(MetNo, CalStep, 7) + MeterData(MetNo, CalStep, 8) + MeterData(MetNo, CalStep, 9)) / 3 'PF
            '    ElseIf MetNo > 0 And ProcessType = 2 Then
            '        MeterData(MetNo, VerfStep, 21) = (MeterData(MetNo, VerfStep, 1) + MeterData(MetNo, VerfStep, 2) + MeterData(MetNo, VerfStep, 3)) / 3 'V
            '        MeterData(MetNo, VerfStep, 25) = (MeterData(MetNo, VerfStep, 4) + MeterData(MetNo, VerfStep, 5) + MeterData(MetNo, VerfStep, 6)) / 3 'A
            '        MeterData(MetNo, VerfStep, 14) = (Math.Abs(MeterData(MetNo, VerfStep, 7)) + Math.Abs(MeterData(MetNo, VerfStep, 8)) + Math.Abs(MeterData(MetNo, VerfStep, 9))) / 3 'PF
            '    End If
            'End If

        Else

            '=========================================================
            ' ET600 STEP 0
            ' COMMON VOLTAGE BLOCK
            '=========================================================
            If ET600ReadStep(MetNo) = 0 Then

                tmData = Mid(tmData, 7)

                start = 1


                ' First 9 ET600 common values
                For i = 1 To 9

                    tinyMasterRawData = Mid(tmData, start + 4, 4) + Mid(tmData, start, 4)

                    binData = LongToBinary(tinyMasterRawData)

                    DecodedValue = CSng(Bin2Float(binData))

                    If ProcessType = 1 Then

                        Select Case i

                            Case 5
                                ' VLN Average
                                MeterData(MetNo, CalStep, 21) = DecodedValue

                            Case 6
                                ' Vr
                                MeterData(MetNo, CalStep, 1) = DecodedValue

                            Case 7
                                ' Vy
                                MeterData(MetNo, CalStep, 2) = DecodedValue

                            Case 8
                                ' Vb
                                MeterData(MetNo, CalStep, 3) = DecodedValue

                            Case 9
                                ' Frequency
                                MeterData(MetNo, CalStep, 29) = DecodedValue

                        End Select


                    ElseIf ProcessType = 2 Then

                        Select Case i

                            Case 5
                                MeterData(MetNo, VerfStep, 21) = DecodedValue

                            Case 6
                                MeterData(MetNo, VerfStep, 1) = DecodedValue

                            Case 7
                                MeterData(MetNo, VerfStep, 2) = DecodedValue

                            Case 8
                                MeterData(MetNo, VerfStep, 3) = DecodedValue

                            Case 9
                                MeterData(MetNo, VerfStep, 29) = DecodedValue

                        End Select

                    End If

                    start = start + 8

                Next

                ' Voltage alone is NOT one complete sample.
                Exit Sub

                '=========================================================
                ' ET600 STEP 1
                '
                ' 4101 BLOCK
                '
                ' R = Physical CH1
                ' Y = Physical CH2
                ' B = Physical CH3
                '=========================================================
            ElseIf ET600ReadStep(MetNo) = 1 Then

                tmData = Mid(tmData, 7)

                start = 1

                For i = 1 To 16

                    tinyMasterRawData = Mid(tmData, start + 4, 4) + Mid(tmData, start, 4)

                    binData = LongToBinary(tinyMasterRawData)

                    DecodedValue = CSng(Bin2Float(binData))

                    If ProcessType = 1 Then

                        Select Case i

                    '-----------------------------------------
                    ' WATTS GROUP 1
                    '-----------------------------------------
                            Case 1
                                MeterData(MetNo, CalStep, 10) = DecodedValue       ' Total

                            Case 2
                                MeterData(MetNo, CalStep, 11) = DecodedValue       ' CH1 / R

                            Case 3
                                MeterData(MetNo, CalStep, 12) = DecodedValue       ' CH2 / Y

                            Case 4
                                MeterData(MetNo, CalStep, 13) = DecodedValue       ' CH3 / B

                    '-----------------------------------------
                    ' PF GROUP 1
                    '-----------------------------------------
                            Case 5
                                MeterData(MetNo, CalStep, 14) = DecodedValue       ' temporary average

                            Case 6
                                MeterData(MetNo, CalStep, 7) = DecodedValue       ' PF CH1

                            Case 7
                                MeterData(MetNo, CalStep, 8) = DecodedValue       ' PF CH2

                            Case 8
                                MeterData(MetNo, CalStep, 9) = DecodedValue       ' PF CH3

                    '-----------------------------------------
                    ' CURRENT GROUP 1
                    '-----------------------------------------
                            Case 13
                                MeterData(MetNo, CalStep, 25) = DecodedValue       ' temporary average

                            Case 14
                                MeterData(MetNo, CalStep, 4) = DecodedValue       ' Current CH1

                            Case 15
                                MeterData(MetNo, CalStep, 5) = DecodedValue       ' Current CH2

                            Case 16
                                MeterData(MetNo, CalStep, 6) = DecodedValue       ' Current CH3

                        End Select

                    ElseIf ProcessType = 2 Then

                        Select Case i

                            Case 1
                                MeterData(MetNo, VerfStep, 10) = DecodedValue

                            Case 2
                                MeterData(MetNo, VerfStep, 11) = DecodedValue

                            Case 3
                                MeterData(MetNo, VerfStep, 12) = DecodedValue

                            Case 4
                                MeterData(MetNo, VerfStep, 13) = DecodedValue


                            Case 5
                                MeterData(MetNo, VerfStep, 14) = DecodedValue

                            Case 6
                                MeterData(MetNo, VerfStep, 7) = DecodedValue

                            Case 7
                                MeterData(MetNo, VerfStep, 8) = DecodedValue

                            Case 8
                                MeterData(MetNo, VerfStep, 9) = DecodedValue

                            Case 13
                                MeterData(MetNo, VerfStep, 25) = DecodedValue

                            Case 14
                                MeterData(MetNo, VerfStep, 4) = DecodedValue

                            Case 15
                                MeterData(MetNo, VerfStep, 5) = DecodedValue

                            Case 16
                                MeterData(MetNo, VerfStep, 6) = DecodedValue


                        End Select

                    End If


                    start = start + 8

                Next
                ' CH1-CH3 is still not a complete 6-channel sample.
                Exit Sub

                '=========================================================
                ' ET600 STEP 2
                '
                ' 4201 BLOCK
                '
                ' R = Physical CH4
                ' Y = Physical CH5
                ' B = Physical CH6
                '=========================================================
            ElseIf ET600ReadStep(MetNo) = 2 Then

                tmData = Mid(tmData, 7)

                start = 1

                For i = 1 To 16

                    tinyMasterRawData = Mid(tmData, start + 4, 4) + Mid(tmData, start, 4)

                    binData = LongToBinary(tinyMasterRawData)

                    DecodedValue = CSng(Bin2Float(binData))

                    If ProcessType = 1 Then

                        Select Case i

                    '-----------------------------------------
                    ' WATTS GROUP 2
                    '-----------------------------------------
                            Case 1
                                MeterData(MetNo, CalStep, 28) = DecodedValue       ' Total group 2

                            Case 2
                                MeterData(MetNo, CalStep, 22) = DecodedValue       ' Watts CH4

                            Case 3
                                MeterData(MetNo, CalStep, 23) = DecodedValue       ' Watts CH5

                            Case 4
                                MeterData(MetNo, CalStep, 24) = DecodedValue       ' Watts CH6

                    '-----------------------------------------
                    ' PF GROUP 2
                    '-----------------------------------------
                            Case 5
                                MeterData(MetNo, CalStep, 27) = DecodedValue       ' PF average group 2

                            Case 6
                                MeterData(MetNo, CalStep, 18) = DecodedValue       ' PF CH4

                            Case 7
                                MeterData(MetNo, CalStep, 19) = DecodedValue       ' PF CH5

                            Case 8
                                MeterData(MetNo, CalStep, 20) = DecodedValue       ' PF CH6

                    '-----------------------------------------
                    ' CURRENT GROUP 2
                    '-----------------------------------------
                            Case 13
                                MeterData(MetNo, CalStep, 26) = DecodedValue       ' Current avg group 2

                            Case 14
                                MeterData(MetNo, CalStep, 15) = DecodedValue       ' Current CH4

                            Case 15
                                MeterData(MetNo, CalStep, 16) = DecodedValue       ' Current CH5

                            Case 16
                                MeterData(MetNo, CalStep, 17) = DecodedValue       ' Current CH6

                        End Select

                    ElseIf ProcessType = 2 Then

                        Select Case i

                            Case 1
                                MeterData(MetNo, VerfStep, 28) = DecodedValue

                            Case 2
                                MeterData(MetNo, VerfStep, 22) = DecodedValue

                            Case 3
                                MeterData(MetNo, VerfStep, 23) = DecodedValue

                            Case 4
                                MeterData(MetNo, VerfStep, 24) = DecodedValue

                            Case 5
                                MeterData(MetNo, VerfStep, 27) = DecodedValue

                            Case 6
                                MeterData(MetNo, VerfStep, 18) = DecodedValue

                            Case 7
                                MeterData(MetNo, VerfStep, 19) = DecodedValue

                            Case 8
                                MeterData(MetNo, VerfStep, 20) = DecodedValue

                            Case 13
                                MeterData(MetNo, VerfStep, 26) = DecodedValue

                            Case 14
                                MeterData(MetNo, VerfStep, 15) = DecodedValue

                            Case 15
                                MeterData(MetNo, VerfStep, 16) = DecodedValue

                            Case 16
                                MeterData(MetNo, VerfStep, 17) = DecodedValue

                        End Select

                    End If

                    start = start + 8

                Next

                '=====================================================
                ' BUILD COMBINED 6-CHANNEL VALUES
                '=====================================================

                If ProcessType = 1 Then

                    '-----------------------------------------
                    ' Current average across CH1 to CH6
                    '-----------------------------------------
                    MeterData(MetNo, CalStep, 25) = (MeterData(MetNo, CalStep, 4) + MeterData(MetNo, CalStep, 5) + MeterData(MetNo, CalStep, 6) + MeterData(MetNo, CalStep, 15) + MeterData(MetNo, CalStep, 16) + MeterData(MetNo, CalStep, 17)) / 6

                    '-----------------------------------------
                    ' PF average across CH1 to CH6
                    '-----------------------------------------
                    MeterData(MetNo, CalStep, 14) = (Math.Abs(MeterData(MetNo, CalStep, 7)) + Math.Abs(MeterData(MetNo, CalStep, 8)) + Math.Abs(MeterData(MetNo, CalStep, 9)) + Math.Abs(MeterData(MetNo, CalStep, 18)) + Math.Abs(MeterData(MetNo, CalStep, 19)) + Math.Abs(MeterData(MetNo, CalStep, 20))) / 6


                ElseIf ProcessType = 2 Then

                    MeterData(MetNo, VerfStep, 25) = (MeterData(MetNo, VerfStep, 4) + MeterData(MetNo, VerfStep, 5) + MeterData(MetNo, VerfStep, 6) + MeterData(MetNo, VerfStep, 15) + MeterData(MetNo, VerfStep, 16) + MeterData(MetNo, VerfStep, 17)) / 6

                    MeterData(MetNo, VerfStep, 14) = (Math.Abs(MeterData(MetNo, VerfStep, 7)) + Math.Abs(MeterData(MetNo, VerfStep, 8)) + Math.Abs(MeterData(MetNo, VerfStep, 9)) + Math.Abs(MeterData(MetNo, VerfStep, 18)) + Math.Abs(MeterData(MetNo, VerfStep, 19)) + Math.Abs(MeterData(MetNo, VerfStep, 20))) / 6

                End If


                '=====================================================
                ' FIRST 12 SETTLING CYCLES:
                ' Decode completed, but don't save sample.
                '=====================================================
                If SkipET600Sample = True Then
                    Exit Sub
                End If


                '=====================================================
                ' DEBUG METER 1
                '=====================================================
                If MetNo = 1 AndAlso ProcessType = 1 Then

                    ErrorEvents(
                        "ET600 6CH" &
                        ", V=" & MeterData(MetNo, CalStep, 21) &
                        ", CH1=" & MeterData(MetNo, CalStep, 4) &
                        ", CH2=" & MeterData(MetNo, CalStep, 5) &
                        ", CH3=" & MeterData(MetNo, CalStep, 6) &
                        ", CH4=" & MeterData(MetNo, CalStep, 15) &
                        ", CH5=" & MeterData(MetNo, CalStep, 16) &
                        ", CH6=" & MeterData(MetNo, CalStep, 17) &
                        ", AVG=" & MeterData(MetNo, CalStep, 25) &
                        ", PF=" & MeterData(MetNo, CalStep, 14) &
                        ", Hz=" & MeterData(MetNo, CalStep, 29)
                    )

                End If


                ' IMPORTANT:
                ' Do NOT Exit Sub here.
                '
                ' Step 2 completes the full ET600 sample.
                ' Continue to the existing:
                '
                ' voltage validation
                ' current validation
                ' PF validation
                ' Sample += 1
                ' CalMeterData / VerMeterData
                ' FillDataGrid()

            End If

        End If


        'If MetNo = 14 Then
        '    MsgBox("s")
        'End If

        If ProcessType = 1 Then
            If MeterData(MetNo, CalStep, 21) <= 100 Then
                Label2.Text = "Volt Error at Meter No." + Str(MetNo)
                If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
                Exit Sub
            End If
        Else
            If MeterData(MetNo, VerfStep, 21) <= 100 Then
                Label2.Text = "Volt Error at Meter No." + Str(MetNo)
                If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
                Exit Sub
            End If
        End If

        If MetNo = 0 Then
            RefErr = 5
            VerErr = 2
        Else
            RefErr = 10
            VerErr = 3
        End If

        'If ProcessType = 1 Or PollType < 6 Then
        '    VoltVal = ((MeterData(MetNo, CalStep, 21) * 100) / CalVolt(CalStep)) - 100
        '    If Math.Abs(VoltVal) > RefErr Then
        '        Label2.Text = "Volt Error at Meter No." + Str(MetNo) + " " + Str(MeterData(MetNo, CalStep, 21))
        '        If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
        '        Exit Sub
        '    End If
        '    If MetNo = 0 Then
        '        AmpVal = ((MeterData(MetNo, CalStep, 25) * 100) / CalAmps(CalStep)) - 100
        '        PFVal = ((Math.Abs(MeterData(MetNo, CalStep, 9)) * 100) / Math.Abs(MeterData(0, CalStep, 9))) - 100
        '    Else
        '        AmpVal = ((MeterData(MetNo, CalStep, 25) * 100) / MeterData(0, CalStep, 25)) - 100
        '        PFVal = ((Math.Abs(MeterData(MetNo, CalStep, 14)) * 100) / Math.Abs(MeterData(0, CalStep, 9))) - 100
        '    End If
        '    If Math.Abs(AmpVal) > RefErr Then
        '        Label2.Text = "Amps Error at Meter No." + Str(MetNo) + " " + Str(AmpVal) + " " + Str(MeterData(MetNo, CalStep, 25)) + "  Ref=" + Str(MeterData(0, CalStep, 25))
        '        ErrorEvents2(Label2.Text)
        '        If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
        '        Exit Sub
        '    End If
        '    If Math.Abs(PFVal) > RefErr And ComboBox1.SelectedIndex = 1 Then
        '        If MetNo = 0 Then
        '            ErrorEvents2("PF Error at Meter No." + Str(MetNo) + " " + Str(MeterData(0, CalStep, 9)))
        '            Label2.Text = "PF Error at Meter No." + Str(MetNo) + " " + Str(MeterData(0, CalStep, 9))
        '        Else
        '            ErrorEvents2(Label2.Text + " " + Str(MeterData(MetNo, CalStep, 14)) + " Ref=" + Str(MeterData(0, CalStep, 9)))
        '            Label2.Text = "PF Error at Meter No." + Str(MetNo) + " " + Str(MeterData(MetNo, CalStep, 9)) + " Ref=" + Str(MeterData(0, CalStep, 9))
        '        End If

        '        If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
        '        Exit Sub
        '    End If
        If ProcessType = 1 OrElse PollType < 6 Then

            If CalStep < 1 OrElse CalStep > 5 Then Exit Sub
            If CalVolt(CalStep) <= 0 OrElse CalAmps(CalStep) <= 0 Then Exit Sub

            VoltVal = ((MeterData(MetNo, CalStep, 21) * 100) / CalVolt(CalStep)) - 100

            If Math.Abs(VoltVal) > RefErr Then
                Label2.Text = "Volt Error at Meter No." & MetNo & " Error=" & VoltVal.ToString("F3") & "% Actual=" & MeterData(MetNo, CalStep, 21)
                ErrorEvents2(Label2.Text)
                If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
                Exit Sub
            End If

            If MetNo = 0 Then

                AmpVal = ((MeterData(0, CalStep, 25) * 100) / CalAmps(CalStep)) - 100

                ErrorEvents("REF CURRENT | Step=" & CalStep & " | Target=" & CalAmps(CalStep) & " | Actual=" & MeterData(0, CalStep, 25) & " | Error=" & AmpVal.ToString("F3") & "%")

                PFVal = 0

                If Double.IsNaN(MeterData(0, CalStep, 9)) OrElse Math.Abs(MeterData(0, CalStep, 9)) < 0.000001 Then
                    Label2.Text = "Invalid Reference PF at Step " & CalStep
                    ErrorEvents2(Label2.Text)
                    Exit Sub
                End If

            Else

                If MeterData(0, CalStep, 25) <= 0 OrElse Math.Abs(MeterData(0, CalStep, 9)) < 0.000001 Then
                    Label2.Text = "Invalid Reference Data at Step " & CalStep
                    ErrorEvents2(Label2.Text)
                    Exit Sub
                End If

                AmpVal = ((MeterData(MetNo, CalStep, 25) * 100) / MeterData(0, CalStep, 25)) - 100
                PFVal = ((Math.Abs(MeterData(MetNo, CalStep, 14)) * 100) / Math.Abs(MeterData(0, CalStep, 9))) - 100

            End If

            If Double.IsNaN(AmpVal) OrElse Double.IsInfinity(AmpVal) OrElse Math.Abs(AmpVal) > RefErr Then
                Label2.Text = "Amps Error at Meter No." & MetNo & " Error=" & AmpVal.ToString("F3") & "% Actual=" & MeterData(MetNo, CalStep, 25) & " Target=" & CalAmps(CalStep)
                ErrorEvents2(Label2.Text)
                If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
                Exit Sub
            End If

            If Math.Abs(PFVal) > RefErr AndAlso ComboBox1.SelectedIndex = 1 Then
                Label2.Text = "PF Error at Meter No." & MetNo & " MUT PF=" & MeterData(MetNo, CalStep, 14) & " Ref PF=" & MeterData(0, CalStep, 9)
                ErrorEvents2(Label2.Text)
                If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
                Exit Sub
            End If
        Else
            VoltVal = ((MeterData(MetNo, VerfStep, 21) * 100) / VerVolt(VerfStep)) - 100
            If Math.Abs(VoltVal) > 3 Then
                Label2.Text = "Step:" + Str(VerfStep) + " Volt Error at Meter No." + Str(MetNo) + " " + Str(MeterData(MetNo, VerfStep, 21))
                If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
                Exit Sub
            End If
            If MetNo = 0 Then
                AmpVal = ((MeterData(MetNo, VerfStep, 25) * 100) / VerAmps(VerfStep)) - 100
                PFVal = ((Math.Abs(MeterData(MetNo, VerfStep, 9)) * 100) / Math.Abs(MeterData(0, VerfStep, 9))) - 100
            Else
                AmpVal = ((MeterData(MetNo, VerfStep, 25) * 100) / VerAmps(VerfStep)) - 100
                PFVal = ((Math.Abs(MeterData(MetNo, VerfStep, 14)) * 100) / Math.Abs(MeterData(0, VerfStep, 9))) - 100
            End If
            If Math.Abs(AmpVal) > VerErr Then
                Label2.Text = "Step:" + Str(VerfStep) + "Sample:" + Str(Sample(MetNo)) + " " + Str(AmpVal) + " Amps Error at Meter No." + Str(MetNo) + " " + " " + Str(MeterData(MetNo, VerfStep, 25)) + " Ref=" + Str(MeterData(0, VerfStep, 25))
                ErrorEvents2(Label2.Text + " " + Str(MeterData(MetNo, VerfStep, 25)) + " Ref=" + Str(MeterData(0, VerfStep, 25)))
                If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
                Exit Sub
            End If
            If Math.Abs(PFVal) > VerErr And ComboBox1.SelectedIndex = 1 Then
                Label2.Text = "Step:" + Str(VerfStep) + "Sample:" + Str(Sample(MetNo)) + " PF Error at Meter No." + Str(MetNo) + " " + Str(MeterData(MetNo, VerfStep, 14)) + " Ref=" + Str(MeterData(0, VerfStep, 9))
                If MetNo = 0 Then
                    ErrorEvents2(Label2.Text + " Ref=" + Str(MeterData(0, VerfStep, 14)))
                Else
                    ErrorEvents2(Label2.Text + " " + Str(MeterData(MetNo, VerfStep, 14)) + " Ref=" + Str(MeterData(0, VerfStep, 9)))
                End If
                If PollType = 6 Then ListBox1.Items.Add(Label2.Text)
                Exit Sub
            End If
        End If

        If ListBox1.Items.Count > 20 Then
            ListBox1.Items.Clear()
            ListBox1.SelectedIndex = ListBox1.Items.Count - 1
        End If

        Sample(MetNo) = Sample(MetNo) + 1
        If Sample(MetNo) > 200 Then Sample(MetNo) = 200

        If ProcessType = 1 Then
            For i = 1 To 29
                CalMeterData(MetNo, i, Sample(MetNo)) = Math.Abs(MeterData(MetNo, CalStep, i))
            Next
        Else
            For i = 1 To 29
                VerMeterData(MetNo, i, Sample(MetNo)) = Math.Abs(MeterData(MetNo, VerfStep, i))
            Next
        End If

        FillDataGrid(MetNo)

        Exit Sub


    End Sub

    Function Bin2Float(ByVal stringdata As String) As Decimal

        Dim i As Integer

        Dim signed_part As String
        Dim exponent_part As String
        Dim floating_part As String

        Dim signed As Integer
        Dim exponent As Integer
        Dim float_digit As Integer

        Dim floating As Double
        Dim float_number As Double
        Dim float_factor As Double

        Try
            signed_part = Mid(stringdata, 1, 1)
            exponent_part = Mid(stringdata, 2, 8)
            floating_part = Mid(stringdata, 10)

            signed = Bin2Dec(signed_part)
            exponent = Bin2Dec(exponent_part)

            For i = 1 To 23
                float_digit = Bin2Dec(Mid$(floating_part, i, 1))
                If float_digit = 1 Then
                    floating = float_digit / (2 ^ i)
                    float_number = float_number + floating
                End If
            Next
            If exponent > 0 Then
                float_factor = 1 + float_number
            ElseIf exponent = 0 Then
                float_factor = float_number
            End If

            If exponent > 0 And exponent < 255 Then
                Bin2Float = ((-1) ^ signed) * (2 ^ (exponent - 127)) * float_factor
            ElseIf exponent = 0 Then
                Bin2Float = ((-1) ^ signed) * (2 ^ -126) * float_factor
            ElseIf exponent = 255 Then
                Bin2Float = 0
            End If

        Catch ex As Exception
            ErrorEvents("Bin2Float : " + ex.Message)
            Bin2Float = 0
        End Try

    End Function

    Private Function LongToBinary(ByVal Data_value As String, Optional ByVal separate_bytes As Boolean = True) As String

        Dim hex_string As String
        Dim digit_num As Integer
        Dim digit_value As Integer
        Dim nibble_string As String
        Dim result_string As String
        Dim factor As Integer
        Dim bit As Integer
        Dim i As Byte

        Try

            hex_string = Data_value
            If Len(hex_string) < 8 Then
                For i = Len(Data_value) + 1 To 8
                    hex_string = Trim("0") + Trim(hex_string)
                Next
            End If

            result_string = ""
            For digit_num = 8 To 1 Step -1
                digit_value = CLng("&H" & Mid$(hex_string, digit_num, 1))

                factor = 1
                nibble_string = ""
                For bit = 3 To 0 Step -1
                    If digit_value And factor Then
                        nibble_string = "1" & nibble_string
                    Else
                        nibble_string = "0" & nibble_string
                    End If
                    factor = factor * 2
                Next bit
                result_string = nibble_string & result_string
            Next digit_num

            If separate_bytes Then
                result_string =
                    Mid$(result_string, 1, 8) & Mid$(result_string, 9, 8) &
                    Mid$(result_string, 17, 8) & Mid$(result_string, 25, 8)
            End If
            LongToBinary = result_string

        Catch ex As Exception
            ErrorEvents("LongToBinary : " + ex.Message)
        End Try

    End Function

    Function Bin2Dec(ByVal stringdata As String) As Long

        Dim n As Integer
        Dim a As Integer
        Dim x As String
        n = Len(stringdata) - 1
        a = n
        Bin2Dec = 0
        Do While n > -1
            x = Mid$(stringdata, ((a + 1) - n), 1)
            Bin2Dec = IIf((x = "1"), Bin2Dec + (2 ^ (n)), Bin2Dec)
            n = n - 1
        Loop

    End Function

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        PollMeters(2)
    End Sub

    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
        PollMeters(3)
    End Sub

    Private Sub Timer4_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer4.Tick
        PollMeters(4)
    End Sub

    Private Sub SourceAxWinsock1_DataArrival(ByVal sender As Object, ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent) Handles SourceAxWinsock1.DataArrival

        Dim Amps As Single
        Dim Volt As Single
        Dim AxData_send As Single
        Dim i As Integer
        Dim rxBuff1 As String = ""
        Dim PF As Single
        Dim bytes As Integer = SourceAxWinsock1.BytesReceived
        Dim comBuffer As Byte() = New Byte(bytes - 1) {}

        SourceAxWinsock1.GetData(comBuffer)
        ErrorEvents(
    "APC SOURCE WRITE" &
    " | Time=" & Now.ToString("HH:mm:ss.fff") &
    " | CalStep=" & CalStep &
    " | Volt=" & Volt &
    " | Amps=" & Amps &
    " | PF=" & PF &
    " | SocketState=" & SourceAxWinsock1.CtlState &
    " | TX=" & BitConverter.ToString(
        System.Text.Encoding.GetEncoding(28591).GetBytes(AxData_send)
    ).Replace("-", "")
)


        For i = 0 To UBound(comBuffer)
            If Len(Hex(comBuffer(i))) = 1 Then
                rxBuff1 = rxBuff1 + "0" + Hex(comBuffer(i))
            Else
                rxBuff1 = rxBuff1 + Hex(comBuffer(i))
            End If
        Next
        ErrorEvents(rxBuff1)


    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click

        Dim comBuffer() As Byte
        ReDim CCWrite(6)
        Dim WS_State As Byte

        startTime = Now
        RefTimer.Enabled = True
        NoOfMUT = ComboBox2.SelectedIndex + 1
        If SourceType = 1 Then
            WS_State = SourceAxWinsock1.CtlState
            If WS_State = 7 Then
                ErrorEvents("Souce IP connected")
                Dim SNGVer As String = "1.45320"
                SourceAxWinsock1.SendData(SNGVer)
                SourceAxWinsock1.GetData(comBuffer)
            Else
                MsgBox("Souce IP  is not coneced. Please check.")
                Exit Sub
            End If
        Else
            Set_APC8000(240, 0, 1)
            Thread.Sleep(15000)
        End If
        EnableTimer()
        PollType = 0
        SNGTimer.Enabled = True
        Button2.Enabled = False
        CalStep = 1

    End Sub

    Private Sub Timer5_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer5.Tick

        PollMeters(5)

    End Sub

    'Private Sub AxWinsock5_DataArrival(ByVal sender As Object, ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent) Handles AxWinsock5.DataArrival

    '    Dim i As Integer
    '    Dim k As UInt16
    '    Dim l As UInt16
    '    Dim Data(5) As Byte
    '    Dim ReceivedCRC As String
    '    Dim CalculatedCRC As String

    '    Dim bytes As Integer = AxWinsock5.BytesReceived
    '    Dim comBuffer As Byte() = New Byte(bytes - 1) {}

    '    If AxWinsock5.BytesReceived = MeterRxBytes(5) Then
    '        AxWinsock5.GetData(comBuffer)
    '        For i = 0 To UBound(comBuffer)
    '            If Len(Hex(comBuffer(i))) = 1 Then
    '                rxBuff(5) = rxBuff(5) + "0" + Hex(comBuffer(i))
    '            Else
    '                rxBuff(5) = rxBuff(5) + Hex(comBuffer(i))
    '            End If
    '        Next
    '    End If

    '    If Len(rxBuff(5)) >= MeterRxBytes(5) * 2 Then
    '        If Val("&h" + Mid(rxBuff(5), 1, 2)) > 247 Then
    '            Exit Sub
    '        End If
    '        ReceivedCRC = Mid(rxBuff(5), (MeterRxBytes(5) * 2 - 3), 4)
    '        k = MeterRxBytes(5) - 3
    '        ReDim Data(k)

    '        l = 0
    '        For i = 1 To Len(Mid(rxBuff(5), 1, (MeterRxBytes(5) * 2 - 4))) Step 2
    '            Data(l) = CByte("&h" + Mid(rxBuff(5), i, 2))
    '            l = l + 1
    '        Next
    '        CalculatedCRC = CRC16(Data)
    '        PollPkts5(rxBuff(5) + ":" + CalculatedCRC + ", M5 PT:" + Str(ProcessType) + ",Sample:" + Str(Sample(5)) + "," + Now.ToString("dd-MM-yy HH:mm"))
    '        If ReceivedCRC <> CalculatedCRC Then
    '            Exit Sub
    '        End If
    '        MeterReadFail(5) = 0
    '        If PollType = 0 Then
    '            MeterSACardNo(5) = "&h" & Mid(rxBuff(5), 71, 8)
    '            CCWrite(5) = 3
    '        ElseIf PollType = 6 Then
    '            If Sample(5) > 100 Then Exit Sub
    '            Decode_Data(rxBuff(5), 5)
    '        ElseIf PollType = 7 Then
    '            CCWrite(5) = 3
    '            ListBox1.Items.Add("Meter5 CC write Sucsess")
    '        ElseIf PollType = 1 Then
    '            CCWrite(5) = 3
    '            ListBox1.Items.Add("Meter5 DCC write Sucsess")
    '        ElseIf PollType = 2 Then
    '            CCWrite(5) = 3
    '            ListBox1.Items.Add("Meter5 DCC Read Sucsess")
    '        ElseIf PollType = 3 Then
    '            CCWrite(5) = 3
    '            ListBox1.Items.Add("Meter5 DC Offset and Hz default write string Completed")
    '        ElseIf PollType = 4 Then
    '            CCWrite(5) = 3
    '            ListBox1.Items.Add("Meter5 Hz calibration Completed")
    '        ElseIf PollType = 5 Then
    '            CCWrite(5) = 3
    '            ListBox1.Items.Add("Meter5 DC Offset calibration Completed")
    '        End If
    '    End If

    'End Sub

    '====================================================================
    ' METER 1 DATA ARRIVAL - ET600 3P
    '====================================================================
    Private Sub AxWinsock1_DataArrival(ByVal sender As Object, ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent) Handles AxWinsock1.DataArrival
        Dim i As Integer
        Dim k As Integer
        Dim l As Integer
        Dim Data(5) As Byte
        Dim ReceivedCRC As String
        Dim CalculatedCRC As String
        Dim CompleteRx As String

        Dim bytes As Integer = AxWinsock1.BytesReceived

        If bytes <= 0 Then Exit Sub

        Dim comBuffer As Byte() = New Byte(bytes - 1) {}

        ' Always read whatever bytes have arrived.
        AxWinsock1.GetData(comBuffer)

        '=============================================
        ' ET600 RAW RECEIVE DIAGNOSTIC
        '=============================================
        ErrorEvents(
    "ET600 RAW RX" &
    " | Time=" & Now.ToString("HH:mm:ss.fff") &
    " | Meter=1" &
    " | PollType=" & PollType &
    " | BytesReceived=" & comBuffer.Length &
    " | ExpectedBytes=" & MeterRxBytes(1) &
    " | Data=" & BitConverter.ToString(comBuffer).Replace("-", "")
)


        '=========================================================
        ' ACCUMULATE TCP CHUNKS
        '=========================================================
        For i = 0 To UBound(comBuffer)

            If Len(Hex(comBuffer(i))) = 1 Then
                rxBuff(1) = rxBuff(1) + "0" + Hex(comBuffer(i))
            Else
                rxBuff(1) = rxBuff(1) + Hex(comBuffer(i))
            End If

        Next

        '=========================================================
        ' WAIT FOR COMPLETE EXPECTED RESPONSE
        '=========================================================
        'If Len(rxBuff(1)) < MeterRxBytes(1) * 2 Then
        '    Exit Sub
        'End If
        If Len(rxBuff(1)) < MeterRxBytes(1) * 2 Then

            ErrorEvents(
        "ET600 RX INCOMPLETE" &
        " | Time=" & Now.ToString("HH:mm:ss.fff") &
        " | Meter=1" &
        " | PollType=" & PollType &
        " | AvailableBytes=" & (Len(rxBuff(1)) \ 2) &
        " | ExpectedBytes=" & MeterRxBytes(1) &
        " | Buffer=" & rxBuff(1)
    )

            Exit Sub
        End If



        CompleteRx =
        rxBuff(1).Substring(0, MeterRxBytes(1) * 2)

        ' Packet copied safely.
        rxBuff(1) = ""

        '=========================================================
        ' DEVICE ID VALIDATION
        '=========================================================
        If Val("&h" + Mid(CompleteRx, 1, 2)) > 247 Then

            If PollType = 6 Then
                ET600ReadWaiting(1) = False
                ET600ReadTimeout(1) = 0
            End If

            Exit Sub

        End If

        '=========================================================
        ' RECEIVED CRC
        '=========================================================
        ReceivedCRC = Mid(CompleteRx, (MeterRxBytes(1) * 2 - 3), 4)

        '=========================================================
        ' PREPARE DATA WITHOUT CRC
        '=========================================================
        k = MeterRxBytes(1) - 3

        ReDim Data(k)

        l = 0

        For i = 1 To (MeterRxBytes(1) * 2 - 4) Step 2

            Data(l) = CByte("&h" + Mid(CompleteRx, i, 2))

            l = l + 1

        Next

        CalculatedCRC = CRC16(Data)

        ErrorEvents(
    "ET600 CRC CHECK" &
    " | Meter=1" &
    " | PollType=" & PollType &
    " | ReceivedCRC=" & ReceivedCRC &
    " | CalculatedCRC=" & CalculatedCRC &
    " | Result=" & If(
        String.Equals(ReceivedCRC, CalculatedCRC,
                      StringComparison.OrdinalIgnoreCase),
        "PASS",
        "FAIL"
    )
)



        PollPkts1("RX: PT:" + Str(PollType) + ", " + CompleteRx + ":" + CalculatedCRC + ",Sample:" + Str(Sample(1)) + "," + Now.ToString("dd-MM-yy HH:mm"))

        '=========================================================
        ' CRC CHECK
        '=========================================================
        If ReceivedCRC <> CalculatedCRC Then

            ErrorEvents("Meter1 CRC Fail. PollType=" & PollType & " Received=" & ReceivedCRC & " Calculated=" & CalculatedCRC)

            If PollType = 6 Then
                ET600ReadWaiting(1) = False
                ET600ReadTimeout(1) = 0
            End If

            Exit Sub

        End If

        MeterReadFail(1) = 0

        '=========================================================
        ' POLL TYPE PROCESSING
        '=========================================================
        If PollType = 0 Then

            MeterSACardNo(1) = "&h" & Mid(CompleteRx, 71, 8)

            ListBox1.Items.Add("Meter1 SA Crad Sl.No. Read Completed")

            CCWrite(1) = 3

        ElseIf PollType = 6 Then

            '=====================================================
            ' ET600 3P LIVE DATA
            '=====================================================

            If Sample(1) > 100 Then

                ET600ReadWaiting(1) = False
                ET600ReadTimeout(1) = 0

                Exit Sub

            End If

            ' Decode according to current ET600ReadStep.
            Decode_Data(CompleteRx, 1)

            ' Response fully received.
            ET600ReadWaiting(1) = False
            ET600ReadTimeout(1) = 0

            '=====================================================
            ' MOVE TO NEXT ET600 BLOCK
            '
            ' Step 0 = common voltage block
            ' 1 -> CH1/CH2/CH3
            ' 2 -> CH4/CH5/CH6
            '=====================================================
            If ET600ReadStep(1) = 0 Then

                ET600ReadStep(1) = 1

            ElseIf ET600ReadStep(1) = 1 Then

                ET600ReadStep(1) = 2

            Else

                ET600ReadStep(1) = 0

            End If

        ElseIf PollType = 7 Then

            CCWrite(1) = 3
            ListBox1.Items.Add("Meter1 CC write Completed")

        ElseIf PollType = 1 Then

            CCWrite(1) = 3
            ListBox1.Items.Add("Meter1 ET600 DCC Write Completed")

            ErrorEvents(
    "ET600 PT1 ACK PROCESSED" &
    " | Meter=1" &
    " | RX=" & CompleteRx &
    " | CCWrite=" & CCWrite(1)
)

        ElseIf PollType = 2 Then

            CCWrite(1) = 3
            ListBox1.Items.Add("Meter1 ET600 DCC Read Completed")

            ErrorEvents("ET600 DCC READ Meter=1 Bytes=" & (Len(CompleteRx) \ 2) & " Expected=" & MeterRxBytes(1))

        ElseIf PollType = 3 Then

            CCWrite(1) = 3
            ListBox1.Items.Add("Meter1 ET600 3P Default Setup 5/5 Write Completed")

        ElseIf PollType = 4 Then

            CCWrite(1) = 3
            ListBox1.Items.Add("Meter1 ET600 Dispatch Serial Write Completed")

        ElseIf PollType = 5 Then

            CCWrite(1) = 3
            ListBox1.Items.Add("Meter1 DC Offset calibration Completed")

        ElseIf PollType = 8 Then

            CCWrite(1) = 3

            ListBox1.Items.Add("Meter1 ET600 CT Ratio 50/5 Write Completed")

            ErrorEvents("Meter1 ET600 CT Ratio 50/5 ACK")

        End If

    End Sub

    Private Sub AxWinsock2_DataArrival(ByVal sender As Object, ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent) Handles AxWinsock2.DataArrival

        Dim i As Integer
        Dim k As Integer
        Dim l As Integer
        Dim Data(5) As Byte
        Dim ReceivedCRC As String
        Dim CalculatedCRC As String
        Dim CompleteRx As String

        Dim bytes As Integer = AxWinsock2.BytesReceived

        If bytes <= 0 Then Exit Sub

        Dim comBuffer As Byte() = New Byte(bytes - 1) {}

        AxWinsock2.GetData(comBuffer)

        For i = 0 To UBound(comBuffer)

            If Len(Hex(comBuffer(i))) = 1 Then
                rxBuff(2) = rxBuff(2) + "0" + Hex(comBuffer(i))
            Else
                rxBuff(2) = rxBuff(2) + Hex(comBuffer(i))
            End If

        Next

        If Len(rxBuff(2)) < MeterRxBytes(2) * 2 Then
            Exit Sub
        End If

        CompleteRx = rxBuff(2).Substring(0, MeterRxBytes(2) * 2)

        rxBuff(2) = ""

        If Val("&h" + Mid(CompleteRx, 1, 2)) > 247 Then

            If PollType = 6 Then
                ET600ReadWaiting(2) = False
                ET600ReadTimeout(2) = 0
            End If

            Exit Sub

        End If

        ReceivedCRC = Mid(CompleteRx, (MeterRxBytes(2) * 2 - 3), 4)

        k = MeterRxBytes(2) - 3

        ReDim Data(k)

        l = 0

        For i = 1 To (MeterRxBytes(2) * 2 - 4) Step 2

            Data(l) = CByte("&h" + Mid(CompleteRx, i, 2))

            l = l + 1

        Next

        CalculatedCRC = CRC16(Data)

        PollPkts2(CompleteRx + ":" + CalculatedCRC + ", M2 PT:" + Str(ProcessType) + ",Sample:" + Str(Sample(2)) + "," + Now.ToString("dd-MM-yy HH:mm"))

        If ReceivedCRC <> CalculatedCRC Then

            ErrorEvents("Meter2 CRC Fail. PollType=" & PollType & " Received=" & ReceivedCRC & " Calculated=" & CalculatedCRC)

            If PollType = 6 Then
                ET600ReadWaiting(2) = False
                ET600ReadTimeout(2) = 0
            End If

            Exit Sub

        End If

        MeterReadFail(2) = 0

        If PollType = 0 Then

            MeterSACardNo(2) = "&h" & Mid(CompleteRx, 71, 8)
            CCWrite(2) = 3
            ListBox1.Items.Add("Meter2 SA Crad Sl.No. Read Completed")

        ElseIf PollType = 6 Then

            If Sample(2) > 100 Then

                ET600ReadWaiting(2) = False
                ET600ReadTimeout(2) = 0

                Exit Sub

            End If

            Decode_Data(CompleteRx, 2)

            ET600ReadWaiting(2) = False
            ET600ReadTimeout(2) = 0

            If ET600ReadStep(2) = 0 Then

                ET600ReadStep(2) = 1

            ElseIf ET600ReadStep(2) = 1 Then
                ET600ReadStep(2) = 2
            Else
                ET600ReadStep(2) = 0
            End If


        ElseIf PollType = 7 Then
            CCWrite(2) = 3
            ListBox1.Items.Add("Meter2 CC write Sucsess")

        ElseIf PollType = 1 Then
            CCWrite(2) = 3
            ListBox1.Items.Add("Meter2 ET600 DCC Write Sucsess")

        ElseIf PollType = 2 Then
            CCWrite(2) = 3
            ListBox1.Items.Add("Meter2 ET600 DCC Read Sucsess")
            ErrorEvents("ET600 DCC READ Meter=2 Bytes=" & (Len(CompleteRx) \ 2) & " Expected=" & MeterRxBytes(2))

        ElseIf PollType = 3 Then
            CCWrite(2) = 3
            ListBox1.Items.Add("Meter2 ET600 3P Default Setup 5/5 Write Completed")

        ElseIf PollType = 4 Then
            CCWrite(2) = 3
            ListBox1.Items.Add("Meter1 ET600 Dispatch Serial Write Completed")

        ElseIf PollType = 5 Then
            CCWrite(2) = 3
            ListBox1.Items.Add("Meter2 DC Offset calibration Completed")

        ElseIf PollType = 8 Then
            CCWrite(2) = 3
            ListBox1.Items.Add("Meter2 ET600 CT Ratio 50/5 Write Completed")
            ErrorEvents("Meter2 ET600 CT Ratio 50/5 ACK")

        End If

    End Sub

    '====================================================================
    ' METER 3 DATA ARRIVAL - ET600 3P
    '====================================================================
    Private Sub AxWinsock3_DataArrival(ByVal sender As Object, ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent) Handles AxWinsock3.DataArrival

        Dim i As Integer
        Dim k As Integer
        Dim l As Integer
        Dim Data(5) As Byte
        Dim ReceivedCRC As String
        Dim CalculatedCRC As String
        Dim CompleteRx As String

        Dim bytes As Integer = AxWinsock3.BytesReceived

        If bytes <= 0 Then Exit Sub

        Dim comBuffer As Byte() = New Byte(bytes - 1) {}

        AxWinsock3.GetData(comBuffer)

        For i = 0 To UBound(comBuffer)

            If Len(Hex(comBuffer(i))) = 1 Then
                rxBuff(3) = rxBuff(3) + "0" + Hex(comBuffer(i))
            Else
                rxBuff(3) = rxBuff(3) + Hex(comBuffer(i))
            End If

        Next

        If Len(rxBuff(3)) < MeterRxBytes(3) * 2 Then
            Exit Sub
        End If

        CompleteRx =
        rxBuff(3).Substring(0, MeterRxBytes(3) * 2)

        rxBuff(3) = ""

        If Val("&h" + Mid(CompleteRx, 1, 2)) > 247 Then

            If PollType = 6 Then
                ET600ReadWaiting(3) = False
                ET600ReadTimeout(3) = 0
            End If

            Exit Sub

        End If

        ReceivedCRC = Mid(CompleteRx, (MeterRxBytes(3) * 2 - 3), 4)

        k = MeterRxBytes(3) - 3

        ReDim Data(k)

        l = 0

        For i = 1 To (MeterRxBytes(3) * 2 - 4) Step 2

            Data(l) = CByte("&h" + Mid(CompleteRx, i, 2))

            l = l + 1

        Next

        CalculatedCRC = CRC16(Data)

        PollPkts3(CompleteRx + ":" + CalculatedCRC + ", M3 PT:" + Str(ProcessType) + ",Sample:" + Str(Sample(3)) + "," + Now.ToString("dd-MM-yy HH:mm"))

        If ReceivedCRC <> CalculatedCRC Then

            ErrorEvents("Meter3 CRC Fail. PollType=" & PollType & " Received=" & ReceivedCRC & " Calculated=" & CalculatedCRC)

            If PollType = 6 Then
                ET600ReadWaiting(3) = False
                ET600ReadTimeout(3) = 0
            End If

            Exit Sub

        End If

        MeterReadFail(3) = 0

        If PollType = 0 Then
            MeterSACardNo(3) = "&h" & Mid(CompleteRx, 71, 8)
            CCWrite(3) = 3
            ListBox1.Items.Add("Meter3 SA Crad Sl.No. Read Completed")

        ElseIf PollType = 6 Then

            If Sample(3) > 100 Then
                ET600ReadWaiting(3) = False
                ET600ReadTimeout(3) = 0
                Exit Sub

            End If

            Decode_Data(CompleteRx, 3)

            ET600ReadWaiting(3) = False
            ET600ReadTimeout(3) = 0

            If ET600ReadStep(3) = 0 Then

                ET600ReadStep(3) = 1

            ElseIf ET600ReadStep(3) = 1 Then
                ET600ReadStep(3) = 2
            Else
                ET600ReadStep(3) = 0
            End If

        ElseIf PollType = 7 Then

            CCWrite(3) = 3
            ListBox1.Items.Add("Meter3 CC write Sucsess")

        ElseIf PollType = 1 Then
            CCWrite(3) = 3
            ListBox1.Items.Add("Meter3 ET600 DCC Write Sucsess")

        ElseIf PollType = 2 Then
            CCWrite(3) = 3
            ListBox1.Items.Add("Meter3 ET600 DCC Read Sucsess")
            ErrorEvents("ET600 DCC READ Meter=3 Bytes=" & (Len(CompleteRx) \ 2) & " Expected=" & MeterRxBytes(3))

        ElseIf PollType = 3 Then
            CCWrite(3) = 3
            ListBox1.Items.Add("Meter3 ET600 3P Default Setup 5/5 Write Completed")

        ElseIf PollType = 4 Then
            CCWrite(3) = 3
            ListBox1.Items.Add("Meter3 ET600 Dispatch Serial Write Completed")

        ElseIf PollType = 5 Then
            CCWrite(3) = 3
            ListBox1.Items.Add("Meter3 DC Offset calibration Completed")

        ElseIf PollType = 8 Then
            CCWrite(3) = 3
            ListBox1.Items.Add("Meter3 ET600 CT Ratio 50/5 Write Completed")
            ErrorEvents("Meter3 ET600 CT Ratio 50/5 ACK")
        End If

    End Sub



    '====================================================================
    ' METER 4 DATA ARRIVAL - ET600 3P
    '====================================================================
    Private Sub AxWinsock4_DataArrival(
    ByVal sender As Object,
    ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent
) Handles AxWinsock4.DataArrival

        Dim i As Integer
        Dim k As Integer
        Dim l As Integer
        Dim Data(5) As Byte
        Dim ReceivedCRC As String
        Dim CalculatedCRC As String
        Dim CompleteRx As String

        Dim bytes As Integer = AxWinsock4.BytesReceived

        If bytes <= 0 Then Exit Sub

        Dim comBuffer As Byte() = New Byte(bytes - 1) {}

        AxWinsock4.GetData(comBuffer)

        For i = 0 To UBound(comBuffer)

            If Len(Hex(comBuffer(i))) = 1 Then
                rxBuff(4) = rxBuff(4) + "0" + Hex(comBuffer(i))
            Else
                rxBuff(4) = rxBuff(4) + Hex(comBuffer(i))
            End If

        Next

        If Len(rxBuff(4)) < MeterRxBytes(4) * 2 Then
            Exit Sub
        End If

        CompleteRx =
        rxBuff(4).Substring(0, MeterRxBytes(4) * 2)

        rxBuff(4) = ""

        If Val("&h" + Mid(CompleteRx, 1, 2)) > 247 Then

            If PollType = 6 Then
                ET600ReadWaiting(4) = False
                ET600ReadTimeout(4) = 0
            End If

            Exit Sub

        End If

        ReceivedCRC =
        Mid(
            CompleteRx,
            (MeterRxBytes(4) * 2 - 3),
            4
        )

        k = MeterRxBytes(4) - 3

        ReDim Data(k)

        l = 0

        For i = 1 To (MeterRxBytes(4) * 2 - 4) Step 2

            Data(l) =
            CByte("&h" + Mid(CompleteRx, i, 2))

            l = l + 1

        Next

        CalculatedCRC = CRC16(Data)

        PollPkts4(
        CompleteRx +
        ":" + CalculatedCRC +
        ", M4 PT:" + Str(ProcessType) +
        ",Sample:" + Str(Sample(4)) +
        "," + Now.ToString("dd-MM-yy HH:mm")
    )

        If ReceivedCRC <> CalculatedCRC Then

            ErrorEvents(
            "Meter4 CRC Fail. PollType=" &
            PollType &
            " Received=" &
            ReceivedCRC &
            " Calculated=" &
            CalculatedCRC
        )

            If PollType = 6 Then
                ET600ReadWaiting(4) = False
                ET600ReadTimeout(4) = 0
            End If

            Exit Sub

        End If

        MeterReadFail(4) = 0

        If PollType = 0 Then

            MeterSACardNo(4) =
            "&h" & Mid(CompleteRx, 71, 8)

            CCWrite(4) = 3

            ListBox1.Items.Add(
            "Meter4 SA Crad Sl.No. Read Completed"
        )


        ElseIf PollType = 6 Then

            If Sample(4) > 100 Then

                ET600ReadWaiting(4) = False
                ET600ReadTimeout(4) = 0

                Exit Sub

            End If

            Decode_Data(CompleteRx, 4)

            ET600ReadWaiting(4) = False
            ET600ReadTimeout(4) = 0

            If ET600ReadStep(4) = 0 Then

                ET600ReadStep(4) = 1

            ElseIf ET600ReadStep(4) = 1 Then

                ET600ReadStep(4) = 2

            Else

                ET600ReadStep(4) = 0

            End If


        ElseIf PollType = 7 Then

            CCWrite(4) = 3
            ListBox1.Items.Add(
            "Meter4 CC write Sucsess"
        )


        ElseIf PollType = 1 Then

            CCWrite(4) = 3
            ListBox1.Items.Add(
            "Meter4 ET600 DCC Write Sucsess"
        )


        ElseIf PollType = 2 Then

            CCWrite(4) = 3
            ListBox1.Items.Add(
            "Meter4 ET600 DCC Read Sucsess"
        )

            ErrorEvents(
            "ET600 DCC READ Meter=4 Bytes=" &
            (Len(CompleteRx) \ 2) &
            " Expected=" &
            MeterRxBytes(4)
        )


        ElseIf PollType = 3 Then

            CCWrite(4) = 3
            ListBox1.Items.Add(
            "Meter4 ET600 3P Default Setup 5/5 Write Completed"
        )


        ElseIf PollType = 4 Then

            CCWrite(4) = 3
            ListBox1.Items.Add(
           "Meter4 ET600 Dispatch Serial Write Completed"
        )


        ElseIf PollType = 5 Then

            CCWrite(4) = 3
            ListBox1.Items.Add(
            "Meter4 DC Offset calibration Completed"
        )

        ElseIf PollType = 8 Then

            CCWrite(4) = 3

            ListBox1.Items.Add(
                "Meter4 ET600 CT Ratio 50/5 Write Completed"
            )

            ErrorEvents(
                "Meter4 ET600 CT Ratio 50/5 ACK"
            )
        End If

    End Sub



    '====================================================================
    ' METER 5 DATA ARRIVAL - ET600 3P
    '====================================================================
    Private Sub AxWinsock5_DataArrival(
    ByVal sender As Object,
    ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent
) Handles AxWinsock5.DataArrival

        Dim i As Integer
        Dim k As Integer
        Dim l As Integer
        Dim Data(5) As Byte
        Dim ReceivedCRC As String
        Dim CalculatedCRC As String
        Dim CompleteRx As String

        Dim bytes As Integer = AxWinsock5.BytesReceived

        If bytes <= 0 Then Exit Sub

        Dim comBuffer As Byte() = New Byte(bytes - 1) {}

        AxWinsock5.GetData(comBuffer)

        For i = 0 To UBound(comBuffer)

            If Len(Hex(comBuffer(i))) = 1 Then
                rxBuff(5) = rxBuff(5) + "0" + Hex(comBuffer(i))
            Else
                rxBuff(5) = rxBuff(5) + Hex(comBuffer(i))
            End If

        Next

        If Len(rxBuff(5)) < MeterRxBytes(5) * 2 Then
            Exit Sub
        End If

        CompleteRx =
        rxBuff(5).Substring(0, MeterRxBytes(5) * 2)

        rxBuff(5) = ""

        If Val("&h" + Mid(CompleteRx, 1, 2)) > 247 Then

            If PollType = 6 Then
                ET600ReadWaiting(5) = False
                ET600ReadTimeout(5) = 0
            End If

            Exit Sub

        End If

        ReceivedCRC =
        Mid(
            CompleteRx,
            (MeterRxBytes(5) * 2 - 3),
            4
        )

        k = MeterRxBytes(5) - 3

        ReDim Data(k)

        l = 0

        For i = 1 To (MeterRxBytes(5) * 2 - 4) Step 2

            Data(l) =
            CByte("&h" + Mid(CompleteRx, i, 2))

            l = l + 1

        Next

        CalculatedCRC = CRC16(Data)

        PollPkts5(
        CompleteRx +
        ":" + CalculatedCRC +
        ", M5 PT:" + Str(ProcessType) +
        ",Sample:" + Str(Sample(5)) +
        "," + Now.ToString("dd-MM-yy HH:mm")
    )

        If ReceivedCRC <> CalculatedCRC Then

            ErrorEvents(
            "Meter5 CRC Fail. PollType=" &
            PollType &
            " Received=" &
            ReceivedCRC &
            " Calculated=" &
            CalculatedCRC
        )

            If PollType = 6 Then
                ET600ReadWaiting(5) = False
                ET600ReadTimeout(5) = 0
            End If

            Exit Sub

        End If

        MeterReadFail(5) = 0

        If PollType = 0 Then

            MeterSACardNo(5) =
            "&h" & Mid(CompleteRx, 71, 8)

            CCWrite(5) = 3


        ElseIf PollType = 6 Then

            If Sample(5) > 100 Then

                ET600ReadWaiting(5) = False
                ET600ReadTimeout(5) = 0

                Exit Sub

            End If

            Decode_Data(CompleteRx, 5)

            ET600ReadWaiting(5) = False
            ET600ReadTimeout(5) = 0

            If ET600ReadStep(5) = 0 Then

                ET600ReadStep(5) = 1

            ElseIf ET600ReadStep(5) = 1 Then

                ET600ReadStep(5) = 2



            Else

                ET600ReadStep(5) = 0

            End If


        ElseIf PollType = 7 Then

            CCWrite(5) = 3
            ListBox1.Items.Add(
            "Meter5 CC write Sucsess"
        )


        ElseIf PollType = 1 Then

            CCWrite(5) = 3
            ListBox1.Items.Add(
            "Meter5 ET600 DCC Write Sucsess"
        )


        ElseIf PollType = 2 Then

            CCWrite(5) = 3
            ListBox1.Items.Add(
            "Meter5 ET600 DCC Read Sucsess"
        )

            ErrorEvents(
            "ET600 DCC READ Meter=5 Bytes=" &
            (Len(CompleteRx) \ 2) &
            " Expected=" &
            MeterRxBytes(5)
        )


        ElseIf PollType = 3 Then

            CCWrite(5) = 3
            ListBox1.Items.Add(
            "Meter5 ET600 3P Default Setup 5/5 Write Completed"
        )


        ElseIf PollType = 4 Then

            CCWrite(5) = 3
            ListBox1.Items.Add(
            "Meter5 ET600 Dispatch Serial Write Completed"
        )


        ElseIf PollType = 5 Then

            CCWrite(5) = 3
            ListBox1.Items.Add(
            "Meter5 DC Offset calibration Completed"
        )
        ElseIf PollType = 8 Then

            CCWrite(5) = 3

            ListBox1.Items.Add(
                "Meter5 ET600 CT Ratio 50/5 Write Completed"
            )

            ErrorEvents(
                "Meter5 ET600 CT Ratio 50/5 ACK"
            )
        End If

    End Sub



    '====================================================================
    ' METER 6 DATA ARRIVAL - ET600 3P
    '====================================================================
    Private Sub AxWinsock6_DataArrival(
    ByVal sender As Object,
    ByVal e As AxMSWinsockLib.DMSWinsockControlEvents_DataArrivalEvent
) Handles AxWinsock6.DataArrival

        Dim i As Integer
        Dim k As Integer
        Dim l As Integer
        Dim Data(5) As Byte
        Dim ReceivedCRC As String
        Dim CalculatedCRC As String
        Dim CompleteRx As String

        Dim bytes As Integer = AxWinsock6.BytesReceived

        If bytes <= 0 Then Exit Sub

        Dim comBuffer As Byte() = New Byte(bytes - 1) {}

        AxWinsock6.GetData(comBuffer)

        For i = 0 To UBound(comBuffer)

            If Len(Hex(comBuffer(i))) = 1 Then
                rxBuff(6) = rxBuff(6) + "0" + Hex(comBuffer(i))
            Else
                rxBuff(6) = rxBuff(6) + Hex(comBuffer(i))
            End If

        Next

        If Len(rxBuff(6)) < MeterRxBytes(6) * 2 Then
            Exit Sub
        End If

        CompleteRx =
        rxBuff(6).Substring(0, MeterRxBytes(6) * 2)

        rxBuff(6) = ""

        If Val("&h" + Mid(CompleteRx, 1, 2)) > 247 Then

            If PollType = 6 Then
                ET600ReadWaiting(6) = False
                ET600ReadTimeout(6) = 0
            End If

            Exit Sub

        End If

        ReceivedCRC =
        Mid(
            CompleteRx,
            (MeterRxBytes(6) * 2 - 3),
            4
        )

        k = MeterRxBytes(6) - 3

        ReDim Data(k)

        l = 0

        For i = 1 To (MeterRxBytes(6) * 2 - 4) Step 2

            Data(l) =
            CByte("&h" + Mid(CompleteRx, i, 2))

            l = l + 1

        Next

        CalculatedCRC = CRC16(Data)

        PollPkts6(
        CompleteRx +
        ":" + CalculatedCRC +
        ", M6 PT:" + Str(ProcessType) +
        ",Sample:" + Str(Sample(6)) +
        "," + Now.ToString("dd-MM-yy HH:mm")
    )

        If ReceivedCRC <> CalculatedCRC Then

            ErrorEvents("Meter6 CRC Fail. PollType=" & PollType & " Received=" & ReceivedCRC & " Calculated=" & CalculatedCRC)

            If PollType = 6 Then
                ET600ReadWaiting(6) = False
                ET600ReadTimeout(6) = 0
            End If

            Exit Sub

        End If

        MeterReadFail(6) = 0

        If PollType = 0 Then

            MeterSACardNo(6) =
            "&h" & Mid(CompleteRx, 71, 8)

            CCWrite(6) = 3


        ElseIf PollType = 6 Then

            If Sample(6) > 100 Then

                ET600ReadWaiting(6) = False
                ET600ReadTimeout(6) = 0

                Exit Sub

            End If

            Decode_Data(CompleteRx, 6)

            ET600ReadWaiting(6) = False
            ET600ReadTimeout(6) = 0

            If ET600ReadStep(6) = 0 Then

                ET600ReadStep(6) = 1

            ElseIf ET600ReadStep(6) = 1 Then

                ET600ReadStep(6) = 2


            Else

                ET600ReadStep(6) = 0

            End If


        ElseIf PollType = 7 Then

            CCWrite(6) = 3
            ListBox1.Items.Add(
            "Meter6 CC write Sucsess"
        )


        ElseIf PollType = 1 Then

            CCWrite(6) = 3
            ListBox1.Items.Add("Meter6 ET600 DCC Write Sucsess")


        ElseIf PollType = 2 Then

            CCWrite(6) = 3
            ListBox1.Items.Add("Meter6 ET600 DCC Read Sucsess")

            ErrorEvents("ET600 DCC READ Meter=6 Bytes=" & (Len(CompleteRx) \ 2) & " Expected=" & MeterRxBytes(6)
        )

        ElseIf PollType = 3 Then
            CCWrite(6) = 3
            ListBox1.Items.Add("Meter6 ET600 3P Default Setup 5/5 Write Completed")

        ElseIf PollType = 4 Then
            CCWrite(6) = 3
            ListBox1.Items.Add("Meter6 ET600 Dispatch Serial Write Completed")

        ElseIf PollType = 5 Then
            CCWrite(6) = 3
            ListBox1.Items.Add("Meter6 DC Offset calibration Completed")
        ElseIf PollType = 8 Then
            CCWrite(6) = 3
            ListBox1.Items.Add("Meter6 ET600 CT Ratio 50/5 Write Completed")
            ErrorEvents("Meter6 ET600 CT Ratio 50/5 ACK")
        End If

    End Sub

    Private Sub Set_APC8000(ByVal Volt As Double, ByVal Amps As Double, ByVal PF As Double)

        Dim AxData_send As String
        Dim Data_send As String
        Dim Data() As Byte
        Dim j As Byte = 0
        Dim CRC As String
        Dim WS_State As Int16
        Dim k As Integer = 0
        Dim PollString As String
        Dim TString As String

        WS_State = SourceAxWinsock1.CtlState
        If WS_State = 7 Then
            PollString = "FF101AF6002448" '6902 36
            TString = dec_to_float(Volt)
            PollString = PollString + Mid(TString, 5, 4) + Mid(TString, 1, 4)
            'TString = dec_to_float(Amps)
            'PollString = PollString + Mid(TString, 5, 4) + Mid(TString, 1, 4)
            '100A APC Source Scaling
            Dim APCAmps As Double = Amps * 0.5
            TString = dec_to_float(APCAmps)
            PollString = PollString + Mid(TString, 5, 4) + Mid(TString, 1, 4)
            ErrorEvents("APC CURRENT | Required=" & Amps & " | Command=" & APCAmps)


            TString = dec_to_float(PF)
            PollString = PollString + Mid(TString, 5, 4) + Mid(TString, 1, 4)

            For i = 1 To 15
                PollString = PollString + "00000000"
            Next

            k = Len(PollString) / 2
            k = k - 1
            ReDim Data(k)

            Data_send = ""
            For i = 1 To Len(PollString) Step 2
                Data_send = Data_send + Chr(Val("&h" + Mid(PollString, i, 2)))
                Data(j) = CByte("&h" + Mid(PollString, i, 2))
                j = j + 1
            Next

            ErrorEvents(PollString + CRC)

            CRC = CRC16(Data)
            AxData_send = Data_send + Chr(Val("&h" + Mid(CRC, 1, 2))) + Chr(Val("&h" + Mid(CRC, 3, 2)))
            SourceAxWinsock1.SendData(AxData_send)
        End If

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click

        End

    End Sub

    Private Sub Timer6_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer6.Tick
        PollMeters(6)
    End Sub

    'Private Sub ComboBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox3.SelectedIndexChanged
    '    Try

    '        Dim con As New SqlConnection("Data Source=192.168.5.200;Initial Catalog=Elmeasure EPS;User ID=sa;Password=mEA9269mK*$47@bAG")

    '        con.Open()

    '        Dim cmd As New SqlCommand("SELECT Slno, IpAddress,RefMeterIPAddress FROM JIG_RefDetails WHERE JigNo=@JigNo ORDER BY Slno", con)

    '        cmd.Parameters.AddWithValue("@JigNo", ComboBox3.Text)

    '        Dim dr As SqlDataReader = cmd.ExecuteReader()



    '        While dr.Read()

    '            Dim MeterNo As Integer = Convert.ToInt32(dr("Slno"))
    '            MeterIP(MeterNo) = dr("IpAddress").ToString()

    '            If RefMeterIP = "" Then
    '                RefMeterIP = dr("RefMeterIPAddress").ToString()
    '            End If

    '        End While

    '        dr.Close()
    '        con.Close()

    '        MessageBox.Show("JIG IP Addresses Loaded")

    '        SocketConnect(0)

    '        For i As Integer = 1 To 6
    '            SocketConnect(i)
    '        Next


    '    Catch ex As Exception

    '        MessageBox.Show(ex.Message)

    '    End Try
    'End Sub

    Private Sub ComboBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox3.SelectedIndexChanged
        Try

            Dim con As New SqlConnection("Data Source=192.168.5.200;Initial Catalog=Elmeasure EPS;User ID=sa;Password=mEA9269mK*$47@bAG")
            'strConnection = "Data Source=192.168.30.2; Initial Catalog=ELmeasure;Integrated Security=False;User ID=sa; Password=Elnetsrv123*"

            con.Open()

            Dim cmd As New SqlCommand("SELECT Slno, IpAddress,RefMeterIPAddress,source_ip FROM JIG_RefDetails WHERE JigNo=@JigNo ORDER BY Slno", con)

            cmd.Parameters.AddWithValue("@JigNo", ComboBox3.Text)

            Dim dr As SqlDataReader = cmd.ExecuteReader()

            While dr.Read()

                Dim MeterNo As Integer = Convert.ToInt32(dr("Slno"))
                MeterIP(MeterNo) = dr("IpAddress").ToString()

                If RefMeterIP = "" Then
                    RefMeterIP = dr("RefMeterIPAddress").ToString()
                End If

                If SourceIP = "" Then
                    SourceIP = dr("source_ip").ToString().Trim()
                End If

            End While

            dr.Close()
            con.Close()
            ConnectSrc()
            Thread.Sleep(500)

            MessageBox.Show("JIG IP Addresses Loaded")

            SocketConnect(0)

            For i As Integer = 1 To 25
                SocketConnect(i)
            Next


        Catch ex As Exception

            MessageBox.Show(ex.Message)

        End Try
    End Sub


End Class
