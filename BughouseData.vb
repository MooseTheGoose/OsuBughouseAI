Imports System
Imports System.Collections.Generic

Namespace BughouseAI
  Class BughouseData
    Public board As Char(,)
    Public enPassant As String
    Public color As Char
    Public castlingRights As String
    Public reserve As List(Of Char)
    Public promotedSquares As List(Of String)
    Public buddy As BughouseData
    Public myTime As Integer
    Public buddyTime As Integer
    Public myOppTime As Integer
    Public buddyOppTime As Integer

    ' Do not change the case of PieceNames and BackRank
    Public Shared ReadOnly PieceNames = "rnbkqp"
    Public Shared ReadOnly BackRank = "rnbqkbnr"
    Public Shared ReadOnly SLComment = ";"
    Public Shared ReadOnly OpenComment = "{"
    Public Shared ReadOnly CloseComment = "}"
    Public Shared ReadOnly CheckmateSuffix = "#"
    Public Shared ReadOnly CheckSuffix = "+"
    Public Shared ReadOnly InvalidSuffix = SLComment
    Public Shared ReadOnly NoSuffix = ""

    ' NOTE: There are some magic numbers which actually
    '       alias NRows and NColumns, especially in FindAttacker.
    '       Oops.
    Public Const NRows = 8
    Public Const NColumns = 8

    Public Sub New()
      Me.board = New Char(NRows - 1, NColumns - 1) {}
      Me.enPassant = "-"
      Me.color = "w"
      Me.castlingRights = "KQkq"

      For index = 0 To BackRank.Length - 1
        Me.board(0, index) = BackRank(index)
        Me.board(1, index) = "p"c
        Me.board(2, index) = " "c
        Me.board(3, index) = " "c
        Me.board(4, index) = " "c
        Me.board(5, index) = " "c
        Me.board(6, index) = "P"c
        Me.board(7, index) = Char.ToUpper(BackRank(index))
      Next
      Me.buddy = Nothing
      Me.reserve = New List(Of Char)()
      Me.promotedSquares = New List(Of String)()
      Me.myTime = 0
      Me.myOppTime = 0
      Me.buddyTime = 0
      Me.buddyOppTime = 0
    End Sub

    Public Sub New(buddy As BughouseData)
      Me.buddy = buddy
      buddy.buddy = Me

      Me.board = New Char(NRows - 1, NColumns - 1) {}
      Me.enPassant = "-"
      Me.color = "w"
      Me.castlingRights = "KQkq"

      For index = 0 To BackRank.Length - 1
        Me.board(0, index) = BackRank(index)
        Me.board(1, index) = "p"c
        Me.board(2, index) = " "c
        Me.board(3, index) = " "c
        Me.board(4, index) = " "c
        Me.board(5, index) = " "c
        Me.board(6, index) = "P"c
        Me.board(7, index) = Char.ToUpper(BackRank(index))
      Next
    End Sub

    ' Return the square of attacker if there is just
    ' one attacker, or empty string if ambiguous
    ' or nonexistant. Color is indicated by case
    ' of piece (Upper for "w"c, lower for "b"c)
    Public Function FindAttacker(piece As Char, destSquare As String, disambiguation As String, capture As Boolean)
      ' Lots of inlining to keep logic simple
      ' Note that there are some cases I optimize
      ' when I know it's impossible to have ambiguities 
      ' (king, pawn w/o capture)
      Dim srcSquare = ""
      Dim trySrc = ""
      Dim dstx, dsty As Integer
      Dim srcy, srcx As Integer
      Dim comparePiece = piece
      Dim pawnSgn = If(color = "b"c, -1, 1)
      Dim pieceCaptured = capture

      piece = Char.ToUpper(piece)
      If destSquare.Length <> 2 Or Not Char.IsUpper(piece) Then
        Return ""
      End If
      dstx = Convert.ToInt32(destSquare(0)) - Convert.ToInt32("a"c)
      dsty = Convert.ToInt32("8"c) - Convert.ToInt32(destSquare(1))
      If dstx >= 8 Or dstx < 0 Or dsty >= 8 Or dsty < 0 Then
        Return ""
      End If
      Dim dstPiece = board(dsty, dstx)
      If (dstPiece <> " "c And Char.IsUpper(dstPiece) = Char.IsUpper(comparePiece)) _
          Or (capture Xor (dstPiece <> " "c Or (piece = "P"c And enPassant = destSquare))) Then
        Return ""
      End If

      If piece = "K"c Then
        srcx = dstx + 1
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx - 1
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx + 1
        srcy = dsty
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx - 1
        srcy = dsty
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx + 1
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx - 1
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
      End If
      If piece = "R"c Or piece = "Q"c Then
        srcx = dstx
        srcy = dsty + 1
        While srcy < 8
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> "" And trySrc.Contains(disambiguation) Then
              srcSquare = trySrc
              disambiguation = ""
              Exit While
            Else
              If disambiguation = "" And srcSquare <> "" Then
                Return ""
              End If
              srcSquare = trySrc
            End If
          ElseIf board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcy += 1
        End While
        srcx = dstx
        srcy = dsty - 1
        While srcy >= 0
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> "" And trySrc.Contains(disambiguation) Then
              srcSquare = trySrc
              disambiguation = ""
              Exit While
            Else
              If disambiguation = "" And srcSquare <> "" Then
                Return ""
              End If
              srcSquare = trySrc
            End If
          ElseIf board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcy -= 1
        End While
        srcx = dstx + 1
        srcy = dsty
        While srcx < 8
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> "" And trySrc.Contains(disambiguation) Then
              srcSquare = trySrc
              disambiguation = ""
              Exit While
            Else
              If disambiguation = "" And srcSquare <> "" Then
                Return ""
              End If
              srcSquare = trySrc
            End If
          ElseIf board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcx += 1
        End While
        srcy = dsty
        srcx = dstx - 1
        While srcx >= 0
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> "" And trySrc.Contains(disambiguation) Then
              srcSquare = trySrc
              disambiguation = ""
              Exit While
            Else
              If disambiguation = "" And srcSquare <> "" Then
                Return ""
              End If
              srcSquare = trySrc
            End If
          ElseIf board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcx -= 1
        End While
      End If
      If piece = "B"c Or piece = "Q"c Then
        srcx = dstx + 1
        srcy = dsty + 1
        While srcx < 8 And srcy < 8
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> "" And trySrc.Contains(disambiguation) Then
              srcSquare = trySrc
              disambiguation = ""
              Exit While
            Else
              If disambiguation = "" And srcSquare <> "" Then
                Return ""
              End If
              srcSquare = trySrc
            End If
          ElseIf board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcx += 1
          srcy += 1
        End While
        srcx = dstx - 1
        srcy = dsty + 1
        While srcx >= 0 And srcy < 8
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> "" And trySrc.Contains(disambiguation) Then
              srcSquare = trySrc
              disambiguation = ""
              Exit While
            Else
              If disambiguation = "" And srcSquare <> "" Then
                Return ""
              End If
              srcSquare = trySrc
            End If
          ElseIf board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcx -= 1
          srcy += 1
        End While
        srcx = dstx + 1
        srcy = dsty - 1
        While srcx < 8 And srcy >= 0
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> "" And trySrc.Contains(disambiguation) Then
              srcSquare = trySrc
              disambiguation = ""
              Exit While
            Else
              If disambiguation = "" And srcSquare <> "" Then
                Return ""
              End If
              srcSquare = trySrc
            End If
          ElseIf board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcx += 1
          srcy -= 1
        End While
        srcx = dstx - 1
        srcy = dsty - 1
        While srcx >= 0 And srcy >= 0
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If disambiguation <> "" And trySrc.Contains(disambiguation) Then
              srcSquare = trySrc
              disambiguation = ""
              Exit While
            Else
              If disambiguation = "" And srcSquare <> "" Then
                Return ""
              End If
              srcSquare = trySrc
            End If
          ElseIf board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcx -= 1
          srcy -= 1
        End While
      End If
      If piece = "N"c Then
        srcx = dstx + 1
        srcy = dsty + 2
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If disambiguation <> "" And trySrc.Contains(disambiguation) Then
            srcSquare = trySrc
            disambiguation = ""
          Else
            If disambiguation = "" And srcSquare <> "" Then
              Return ""
            End If
            srcSquare = trySrc
          End If
        End If
        srcx = dstx - 1
        srcy = dsty + 2
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If disambiguation <> "" And trySrc.Contains(disambiguation) Then
            srcSquare = trySrc
            disambiguation = ""
          Else
            If disambiguation = "" And srcSquare <> "" Then
              Return ""
            End If
            srcSquare = trySrc
          End If
        End If
        srcx = dstx + 1
        srcy = dsty - 2
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If disambiguation <> "" And trySrc.Contains(disambiguation) Then
            srcSquare = trySrc
            disambiguation = ""
          Else
            If disambiguation = "" And srcSquare <> "" Then
              Return ""
            End If
            srcSquare = trySrc
          End If
        End If
        srcx = dstx - 1
        srcy = dsty - 2
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If disambiguation <> "" And trySrc.Contains(disambiguation) Then
            srcSquare = trySrc
            disambiguation = ""
          Else
            If disambiguation = "" And srcSquare <> "" Then
              Return ""
            End If
            srcSquare = trySrc
          End If
        End If
        srcx = dstx + 2
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If disambiguation <> Nothing And trySrc.Contains(disambiguation) Then
            srcSquare = trySrc
            disambiguation = ""
          Else
            If disambiguation = "" And srcSquare <> "" Then
              Return ""
            End If
            srcSquare = trySrc
          End If
        End If
        srcx = dstx - 2
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If disambiguation <> Nothing And trySrc.Contains(disambiguation) Then
            srcSquare = trySrc
            disambiguation = ""
          Else
            If disambiguation = "" And srcSquare <> "" Then
              Return ""
            End If
            srcSquare = trySrc
          End If
        End If
        srcx = dstx + 2
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If disambiguation <> Nothing And trySrc.Contains(disambiguation) Then
            srcSquare = trySrc
            disambiguation = ""
          Else
            If disambiguation = "" And srcSquare <> "" Then
              Return ""
            End If
            srcSquare = trySrc
          End If
        End If
        srcx = dstx - 2
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If disambiguation <> Nothing And trySrc.Contains(disambiguation) Then
            srcSquare = trySrc
            disambiguation = ""
          Else
            If disambiguation = "" And srcSquare <> "" Then
              Return ""
            End If
            srcSquare = trySrc
          End If
        End If
      End If
      If piece = "P"c Then
        If pieceCaptured Then
          If disambiguation.Length <> 1 OrElse (disambiguation(0) < "a"c Or disambiguation(0) > "h"c) Then
            Return ""
          End If
          Dim pawnDisambiguation = disambiguation(0)
          srcx = dstx + pawnSgn
          srcy = dsty + pawnSgn
          If ((srcx >= 0 And srcx < 8) AndAlso board(srcy, srcx) = comparePiece)  _
             And Convert.ToChar(Convert.ToInt32("a"c) + srcx) = pawnDisambiguation Then
            disambiguation = ""
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          srcx = dstx - pawnSgn
          srcy = dsty + pawnSgn
          If ((srcx >= 0 And srcx < 8) AndAlso board(srcy, srcx) = comparePiece) _
             And Convert.ToChar(Convert.ToInt32("a"c) + srcx) = pawnDisambiguation Then
            disambiguation = ""
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          If enPassant = destSquare Then
            srcx = dstx + pawnSgn
            srcy = dsty
            If ((srcx >= 0 And srcx < 8) AndAlso board(srcy, srcx) = comparePiece) _
             And Convert.ToChar(Convert.ToInt32("a"c) + srcx) = pawnDisambiguation Then
              disambiguation = ""
              srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            End If
            srcx = dstx - pawnSgn
            srcy = dsty
            If ((srcx >= 0 And srcx < 8) AndAlso board(srcy, srcx) = comparePiece) _
             And Convert.ToChar(Convert.ToInt32("a"c) + srcx) = pawnDisambiguation Then
              disambiguation = ""
              srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            End If
          End If
        Else
          srcx = dstx
          srcy = dsty + pawnSgn
          If board(srcy, srcx) = comparePiece Then
            srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          End If
          If (dsty = 4 And comparePiece = "P"c) Or (dsty = 3 And comparePiece = "p"c) Then
            srcy += pawnSgn
            If board(srcy, srcx) = comparePiece Then
              srcSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            End If
          End If
          If board(dsty, dstx) <> " "c Then
            Return ""
          End If
        End If
      End If

      If disambiguation <> "" Or srcSquare.Length <> 2 Then
        Return ""
      End If
      Return srcSquare
    End Function

    ' Return InvalidSuffix iff move can't legally made.
    ' Otherwise, return the SAN suffix ("#", "+"), 
    ' or special string for no suffix ("", probably).
    ' This is in SAN notation minus check(mate) suffix.
    ' If not, don't change underlying struct.
    Public Function MakeMove(sanNoSuffix As String)
      Dim piece = Convert.ToChar(0)
      Dim disambiguation As String = ""
      Dim srcSquare As String = ""
      Dim destSquare As String = ""
      Dim promotionPiece = Convert.ToChar(0)
      Dim saveCapture = " "c
      Dim enemyPieces = If(color = "w"c, PieceNames, PieceNames.ToUpper())
      Dim friendlyPieces = If(color = "w"c, PieceNames.ToUpper(), PieceNames)
      Dim pieceDropped = False
      Dim pieceCaptured = False

      Dim index = 0
      While index < sanNoSuffix.Length AndAlso Char.IsWhiteSpace(sanNoSuffix(index))
        index += 1
      End While

      Dim friendKingPiece = If(color = "w"c, "K"c, "k"c)
      Dim enemyKingPiece = If(color = "w"c, "k"c, "K"c)
      Dim friendKingSquare = ""
      Dim enemyKingSquare = ""
      For i = 0 To NRows - 1
        For j = 0 To NColumns - 1
          If board(i, j) = friendKingPiece Then
            friendKingSquare = Convert.ToChar(Convert.ToInt32("a"c) + j) + Convert.ToString(8 - i)
          ElseIf board(i, j) = enemyKingPiece Then
            enemyKingSquare = Convert.ToChar(Convert.ToInt32("a"c) + j) + Convert.ToString(8 - i)
          End If
        Next
      Next

      ' Try to get castling done right off the bat.
      Select Case sanNoSuffix
        Case "O-O"
          Return InvalidSuffix
          Dim kingside = If(color = "w"c, "K", "k"c)
          Dim homeRank = If(color = "w"c, 7, 0)
          If Not castlingRights.Contains(kingside) Then
            Return InvalidSuffix
          End If
          GoTo CheckCheckAndMate
        Case "O-O-O"
          Return InvalidSuffix
          Dim queenside = If(color = "w"c, "Q"c, "q"c)
          Dim homeRank = If(color = "w"c, 7, 0)
          If Not castlingRights.Contains(queenside) Then
            Return InvalidSuffix
          End If
          GoTo CheckCheckAndMate
      End Select

      ' First, get the piece specified, if it is specified
      ' Note that 'P' is normally not specified, but in Bughouse,
      ' 'P' is used to denote a pawn drop (i.e. P@e5)
      If index < sanNoSuffix.Length And PieceNames.ToUpper.Contains(sanNoSuffix(index)) Then
        piece = sanNoSuffix(index)
        index += 1
      End If

      ' Get the disambiguation, if it exists.
      ' Note that the disambiguation may be mistaken
      ' for the destination square. This will be fixed up
      ' later.
      If index < sanNoSuffix.Length AndAlso (sanNoSuffix(index) >= "a"c And sanNoSuffix(index) <= "h") Then
        disambiguation += sanNoSuffix(index)
        index += 1
      End If
      If index < sanNoSuffix.Length AndAlso (sanNoSuffix(index) >= "1"c And sanNoSuffix(index) <= "8"c) Then
        disambiguation += sanNoSuffix(index)
        index += 1
      End If

      ' Get whether the piece was dropped or not
      If index < sanNoSuffix.Length AndAlso sanNoSuffix(index) = "@"c Then
        pieceDropped = True
        index += 1
      End If

      ' Get whether a piece was captured or not.
      If index < sanNoSuffix.Length AndAlso sanNoSuffix(index) = "x"c Then
        pieceCaptured = True
        index += 1
      End If

      ' Get the destination square, or fix up disambiguation
      If index + 1 < sanNoSuffix.Length AndAlso (sanNoSuffix(index) >= "a"c And sanNoSuffix(index) <= "h"c _
         And sanNoSuffix(index + 1) >= "1"c And sanNoSuffix(index + 1) <= "8"c) Then
        destSquare = sanNoSuffix.Substring(index, 2)
        index += 2
      Else
        If disambiguation.Length <> 2 Then
          Return InvalidSuffix
        End If
        destSquare = disambiguation
        disambiguation = ""
      End If


      ' Get whether a pawn was promoted
      If index + 1 < sanNoSuffix.Length AndAlso sanNoSuffix(index) = "="c Then
        If Not PieceNames.ToUpper.Contains(sanNoSuffix(index + 1)) _
           Or sanNoSuffix(index + 1) = "P"c Or sanNoSuffix(index + 1) = "K"c Then
          Return InvalidSuffix
        End If
        promotionPiece = sanNoSuffix(index + 1)
        index += 2
      End If

      While index < sanNoSuffix.Length AndAlso Char.IsWhiteSpace(sanNoSuffix(index))
        index += 1
      End While
      If index <> sanNoSuffix.Length Then
        Return InvalidSuffix
      End If

      If pieceDropped Then
        Dim checkReserves = buddy.reserve
        Dim checkReservesPiece = If(color = "w"c, piece, Char.ToLower(piece))
        Dim dropx = Convert.ToInt32(destSquare(0)) - Convert.ToInt32("a"c)
        Dim dropy = Convert.ToInt32("8"c) - Convert.ToInt32(destSquare(1))
        If piece = Convert.ToChar(0) Or disambiguation <> "" Or pieceCaptured  _
             Or promotionPiece <> Convert.ToChar(0) Or Not checkReserves.Contains(checkReservesPiece) _ 
             Or (piece = "P"c And (dropy = 0 Or dropy = 7)) _
             Or ((dropx < 0 Or dropx >= 8 Or dropy < 0 Or dropy >= 8) OrElse board(dropy, dropx) <> " "c)Then
          Return InvalidSuffix
        End If
        srcSquare = destSquare
      Else
        If piece = "P"c Then
          Return InvalidSuffix
        ElseIf piece = Convert.ToChar(0) Then
          piece = "P"c
        End If
        Dim findPiece = If(color = "w"c, piece, Char.ToLower(piece))
        srcSquare = FindAttacker(findPiece, destSquare, disambiguation, pieceCaptured)
        If srcSquare = "" Then
          Return InvalidSuffix
        End If
      End If

      ' Do the move
      Dim srcx = Convert.ToInt32(Convert.ToInt32(srcSquare(0)) - Convert.ToInt32("a"c))
      Dim srcy = Convert.ToInt32("8"c) - Convert.ToInt32(srcSquare(1))
      Dim dstx = Convert.ToInt32(Convert.ToInt32(destSquare(0)) - Convert.ToInt32("a"c))
      Dim dsty = Convert.ToInt32("8"c) - Convert.ToInt32(destSquare(1))
      saveCapture = board(dsty, dstx)
      board(dsty, dstx) = board(srcy, srcx)
      board(srcy, srcx) = " "c
      If destSquare = enPassant And pieceCaptured And piece = "P"c Then
        Dim pawnSgn = If(color = "w"c, 1, -1)
        saveCapture = board(dsty + pawnSgn, dstx)
        board(dsty + pawnSgn, dstx) = " "c
      End If
      If piece = "P"c And (dsty = 7 Or dsty = 0) Then
        If promotionPiece = Convert.ToChar(0) Then
          board(srcy, srcx) = board(dsty, dstx)
          board(dsty, dstx) = saveCapture
          Return InvalidSuffix
        End If
        board(dsty, dstx) = If(color = "w"c, promotionPiece, Char.ToLower(promotionPiece))
      ElseIf promotionPiece <> Convert.ToChar(0) Then
        board(srcy, srcx) = board(dsty, dstx)
        board(dsty, dstx) = saveCapture
        Return InvalidSuffix        
      End If
      If pieceDropped Then
        buddy.reserve.Remove(board(dsty, dstx))
      End If

      ' Check if the current color's king is in check (Unroll if the king is in check)
      ' Fun fact: Moving a friendly piece will have at most one piece attacking
      ' the king, which means no ambiguities
      Dim kingInCheck = False
      For Each epiece In enemyPieces
        kingInCheck = (FindAttacker(epiece, friendKingSquare, "", True) <> "")
        If kingInCheck Then
          board(srcy, srcx) = board(dsty, dstx)
          If destSquare = enPassant And pieceCaptured And piece = "P"c Then
            Dim pawnSgn = If(color = "w"c, 1, -1)
            board(dsty + pawnSgn, dstx) = saveCapture
          Else
            board(dsty, dstx) = saveCapture
          End If
          If piece = "P"c And (dsty = 7 Or dsty = 0) Then
            board(srcy, srcx) = If(color = "w"c, "P"c, "p"c)
          End If
          If pieceDropped Then
            buddy.reserve.Add(board(srcy, srcx))
          End If
          Return InvalidSuffix
        End If
      Next

      ' Commit the move
      enPassant = "-"
      If piece = "P"c Then
        If (dsty - srcy) * (dsty - srcy) = 4 Then
          Dim pawnSgn = If(color = "w"c, 1, -1)
          enPassant = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 + pawnSgn - srcy)
        ElseIf promotionPiece <> Convert.ToChar(0) Then
          promotedSquares.Add(destSquare)
        End If
      ElseIf piece = "R"c Then
        Dim friendQueenPiece = If(color = "w"c, "Q"c, "q"c)
        Dim friendRank = If(color = "w"c, 7, 0)
        If srcx = 0 And friendRank = srcy Then
          castlingRights = castlingRights.Replace(friendQueenPiece.ToString(), "")
        ElseIf srcx = 7 And friendRank = srcy Then
          castlingRights = castlingRights.Replace(friendKingPiece.ToString(), "")
        End If
      ElseIf pieceCaptured And saveCapture = "R"c
        Dim enemyQueenPiece = If(color = "w"c, "q"c, "Q"c)
        DIm enemyRank = If(color = "w"c, 0, 7)
        If dstx = 0 And dsty = enemyRank Then
          castlingRights = castlingRights.Replace(enemyQueenPiece.ToString(), "")
        ElseIf dstx = 8 And dsty = enemyRank Then
          castlingRights = castlingRights.Replace(enemyKingPiece.ToString(), "")
        End If
      ElseIf piece = "K"c Then
        Dim friendQueenPiece = If(color = "w"c, "Q"c, "q"c)
        castlingRights = castlingRights.Replace(friendQueenPiece.ToString(), "")
        castlingRights = castlingRights.Replace(friendKingPiece.ToString(), "")
      End If
      If castlingRights = "" Then
        castlingRights = "-"
      End If
      If pieceCaptured Then
        Dim reservePiece = If(color = "w"c, piece, Char.ToLower(piece))
        For i = 0 To promotedSquares.Count - 1
          If promotedSquares(i) = destSquare Then
            promotedSquares(i) = promotedSquares(promotedSquares.Count - 1)
            promotedSquares.RemoveAt(promotedSquares.Count - 1)
            reservePiece = If(color = "w"c, "P"c, "p"c)
            Exit For
          End If
        Next
        reserve.Add(reservePiece)
      End If
      For i = 0 To promotedSquares.Count - 1
        If promotedSquares(i) = srcSquare Then
          promotedSquares(i) = destSquare
          Exit For
        End If
      Next
      color = If(color = "w"c, "b"c, "w"c)
      ' Then, check for both check and checkmate to get suffix
      ' Do this by temporarily switching colors and checking if there
      ' is an attacker on the king, and if we can move out of the way.
      ' In either case, switch the colors back and see if you can off
      ' the sole attacker, if present, or if you're just screwed.
CheckCheckAndMate:
      Return ""
    End Function
  End Class
End Namespace
