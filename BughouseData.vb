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

    Public Sub New(buddy As BughouseData)
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
      Me.buddy = buddy
      Me.reserve = New List(Of Char)()
      Me.promotedSquares = New List(Of String)()
    End Sub

    ' Return the concatenated squares of the pieces who matches piece
    ' and disambiguation that can access the square, or "" if none. 
    ' Also assume the piece could be anywhere (including pawns).
    ' Capture is used for pawns. The burden is on other procedures for
    ' verifying disambiguation if needed, and that pawns aren't on home ranks.
    ' This leniency makes this method sufficient for checking checks and mates.
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

      If destSquare.Length <> 2 Or Not PieceNames.ToUpper.Contains(piece) Then
        Return ""
      End If
      dstx = Convert.ToInt32(destSquare(0)) - Convert.ToInt32("a"c)
      dsty = Convert.ToInt32("8"c) - Convert.ToInt32(destSquare(1))
      If dstx >= 8 Or dstx < 0 Or dsty >= 8 Or dsty < 0 Then
        Return ""
      End If

      If piece = "K"c Then
        srcx = dstx + 1
        srcy = dsty + 1
        trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
              AndAlso trySrc.Contains(disambiguation) Then
          srcSquare += Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
              AndAlso trySrc.Contains(disambiguation) Then
          srcSquare += Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx - 1
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
              AndAlso trySrc.Contains(disambiguation) Then
          srcSquare += Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx + 1
        srcy = dsty
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
              AndAlso trySrc.Contains(disambiguation) Then
          srcSquare += Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx - 1
        srcy = dsty
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
              AndAlso trySrc.Contains(disambiguation) Then
          srcSquare += Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx + 1
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
              AndAlso trySrc.Contains(disambiguation) Then
          srcSquare += Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
              AndAlso trySrc.Contains(disambiguation) Then
          srcSquare += Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
        srcx = dstx - 1
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
              AndAlso trySrc.Contains(disambiguation) Then
          srcSquare += Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
        End If
      End If
      If piece = "R"c Or piece = "Q"c Then
        srcx = dstx
        srcy = dsty + 1
        While srcy < 8
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
          If board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcy += 1
        End While
        srcx = dstx
        srcy = dsty - 1
        While srcy >= 0
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
          If board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcy -= 1
        End While
        srcx = dstx + 1
        srcy = dsty
        While srcx < 8
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
          If board(srcy, srcx) <> " "c Then
            Exit While
          End If
          srcx += 1
        End While
        srcy = dsty
        srcx = dstx - 1
        While srcx >= 0
          If board(srcy, srcx) = comparePiece Then
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
          If board(srcy, srcx) <> " "c Then
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
            If trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
          If board(srcy, srcx) <> " "c Then
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
            If trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
          If board(srcy, srcx) <> " "c Then
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
            If trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
          If board(srcy, srcx) <> " "c Then
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
            If trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
          If board(srcy, srcx) <> " "c Then
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
          If trySrc.Contains(disambiguation)
            srcSquare += trySrc
          End If
        End If
        srcx = dstx - 1
        srcy = dsty + 2
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If trySrc.Contains(disambiguation)
            srcSquare += trySrc
          End If
        End If
        srcx = dstx + 1
        srcy = dsty - 2
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If trySrc.Contains(disambiguation)
            srcSquare += trySrc
          End If
        End If
        srcx = dstx - 1
        srcy = dsty - 2
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If trySrc.Contains(disambiguation)
            srcSquare += trySrc
          End If
        End If
        srcx = dstx + 2
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If trySrc.Contains(disambiguation)
            srcSquare += trySrc
          End If
        End If
        srcx = dstx - 2
        srcy = dsty + 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If trySrc.Contains(disambiguation)
            srcSquare += trySrc
          End If
        End If
        srcx = dstx + 2
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If trySrc.Contains(disambiguation)
            srcSquare += trySrc
          End If
        End If
        srcx = dstx - 2
        srcy = dsty - 1
        If (srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece Then
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If trySrc.Contains(disambiguation)
            srcSquare += trySrc
          End If
        End If
      End If
      If piece = "P"c Then
        If pieceCaptured Then
          srcx = dstx + pawnSgn
          srcy = dsty + pawnSgn
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If ((srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) _
               AndAlso board(srcy, srcx) = comparePiece) AndAlso trySrc.Contains(disambiguation) Then
            srcSquare += trySrc
          End If
          srcx = dstx - pawnSgn
          srcy = dsty + pawnSgn
          If ((srcx >= 0 And srcx < 8) AndAlso (srcy >= 0 And srcy < 8) _
               AndAlso board(srcy, srcx) = comparePiece) AndAlso trySrc.Contains(disambiguation) Then
            srcSquare += trySrc
          End If
        Else
          srcx = dstx
          srcy = dsty + pawnSgn
          trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
          If (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
               AndAlso trySrc.Contains(disambiguation) Then
            srcSquare += trySrc
          End If
          If (dsty = 4 And comparePiece = "P"c) Or (dsty = 3 And comparePiece = "p"c) _
               And trySrc.Contains(disambiguation) Then
            srcy += pawnSgn
            trySrc = Convert.ToChar(Convert.ToInt32("a"c) + srcx) + Convert.ToString(8 - srcy)
            If (srcy >= 0 And srcy < 8) AndAlso board(srcy, srcx) = comparePiece _
                 AndAlso trySrc.Contains(disambiguation) Then
              srcSquare += trySrc
            End If
          End If
        End If
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
      Dim friendPieces = If(color = "w"c, PieceNames.ToUpper(), PieceNames)
      Dim pieceDropped = False
      Dim pieceCaptured = False
      Dim testSquare = ""

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
          Dim kingside = If(color = "w"c, "K", "k"c)
          Dim homeRank = If(color = "w"c, 7, 0)
          If Not castlingRights.Contains(kingside) Then
            Return InvalidSuffix
          End If
          For file = 4 To 6
            If board(homeRank, file) <> " "c And board(homeRank, file) <> friendKingPiece Then
              Return InvalidSuffix
            End If
            testSquare = Convert.ToChar(Convert.ToInt32("a"c) + file) + Convert.ToString(8 - homeRank)
            For Each epiece In enemyPieces
              If FindAttacker(epiece, testSquare, "", True) <> "" Then
                Return InvalidSuffix
              End If
            Next
          Next
          board(homeRank, 6) = board(homeRank, 4)
          board(homeRank, 5) = board(homeRank, 7)
          board(homeRank, 4) = " "c
          board(homeRank, 7) = " "c
          Dim friendlyQueen = If(color = "w"c, "Q", "q")
          castlingRights = castlingRights.Replace(kingside.ToString(), "").Replace(friendlyQueen, "")
          If castlingRights = "" Then
            castlingRights = "-"
          End If
          enPassant = "-"
          color = If(color = "w"c, "b"c, "w"c)
          GoTo CheckCheckAndMate
        Case "O-O-O"
          Dim queenside = If(color = "w"c, "Q"c, "q"c)
          Dim homeRank = If(color = "w"c, 7, 0)
          If Not castlingRights.Contains(queenside) Then
            Return InvalidSuffix
          End If
          For file = 2 To 4
            If board(homeRank, file) <> " "c And board(homeRank, file) <> friendKingPiece Then
              Return InvalidSuffix
            End If
            testSquare = Convert.ToChar(Convert.ToInt32("a"c) + file) + Convert.ToString(8 - homeRank)
            For Each epiece In enemyPieces
              If FindAttacker(epiece, testSquare, "", True) Then
                Return InvalidSuffix
              End If
            Next
          Next
          board(homeRank, 2) = board(homeRank, 4)
          board(homeRank, 3) = board(homeRank, 0)
          board(homeRank, 4) = " "c
          board(homeRank, 0) = " "c
          Dim friendlyKing = If(color = "w"c, "K", "k")
          castlingRights = castlingRights.Replace(queenside.ToString(), "").Replace(friendlyKing, "")
          If castlingRights = "" Then
            castlingRights = "-"
          End If
          enPassant = "-"
          color = If(color = "w"c, "b"c, "w"c)
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

      Dim dstx = Convert.ToInt32(Convert.ToInt32(destSquare(0)) - Convert.ToInt32("a"c))
      Dim dsty = Convert.ToInt32("8"c) - Convert.ToInt32(destSquare(1))
      Dim dstPiece = board(dsty, dstx)
      If pieceDropped Then
        Dim checkReserves = buddy.reserve
        Dim checkReservesPiece = If(color = "w"c, piece, Char.ToLower(piece))
        Dim dropx = dstx
        Dim dropy = dsty
        If piece = Convert.ToChar(0) Or disambiguation <> "" Or pieceCaptured _
             Or promotionPiece <> Convert.ToChar(0) Or Not checkReserves.Contains(checkReservesPiece) _
             Or (piece = "P"c And (dropy = 0 Or dropy = 7)) _
             Or ((dropx < 0 Or dropx >= 8 Or dropy < 0 Or dropy >= 8) OrElse board(dropy, dropx) <> " "c) Then
          Return InvalidSuffix
        End If
        srcSquare = destSquare
      Else
        If piece = "P"c Then
          Return InvalidSuffix
        ElseIf piece = Convert.ToChar(0) Then
          piece = "P"c
        End If
        If piece = "P"c And pieceCaptured And (disambiguation.Length <> 1 _
              OrElse disambiguation(0) < "a"c OrElse disambiguation(0) > "h"c) Then
          Return InvalidSuffix
        End If
        Dim findPiece = If(color = "w"c, piece, Char.ToLower(piece))
        If (dstPiece <> " "c And Char.IsUpper(dstPiece) = Char.IsUpper(findPiece)) _
            Or (pieceCaptured Xor (dstPiece <> " "c Or (piece = "P"c And enPassant = destSquare))) Then
          Return InvalidSuffix
        End If

        srcSquare = FindAttacker(findPiece, destSquare, "", pieceCaptured)
        If srcSquare.Length = 2 And disambiguation <> "" Then
          Return InvalidSuffix
        End If
        If srcSquare.Length > 2 Then
          For Each file In "abcdefgh"
            srcSquare = FindAttacker(findPiece, destSquare, file.ToString(), pieceCaptured)
            If srcSquare.Length = 2 And disambiguation <> file.ToString() Then
              Return InvalidSuffix
            ElseIf srcSquare.Length > 2 Then
              Exit For
            End If
          Next
          For Each rank In "12345678"
            srcSquare = FindAttacker(findPiece, destSquare, rank.ToString(), pieceCaptured)
            If srcSquare.Length = 2 And disambiguation <> rank.ToString() Then
              Return InvalidSuffix
            ElseIf srcSquare.Length > 2 Then
              Exit For
            End If
          Next
          For Each rank In "12345678"
            For Each file In "abcdefgh"
              srcSquare = FindAttacker(findPiece, destSquare, file + rank.ToString(), pieceCaptured)
              If srcSquare.Length = 2 And disambiguation <> (file + rank.ToString()) Then
                Return InvalidSuffix
              ElseIf srcSquare.Length > 2 Then
                Exit For
              End If
            Next
          Next
        End If
        If srcSquare.Length <> 2 Then
          Return InvalidSuffix
        End If
      End If

      ' Do the move
      Dim srcx = Convert.ToInt32(Convert.ToInt32(srcSquare(0)) - Convert.ToInt32("a"c))
      Dim srcy = Convert.ToInt32("8"c) - Convert.ToInt32(srcSquare(1))
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
        board(dsty, dstx) = If(color = "w"c, piece, Char.ToLower(piece))
      End If

      ' Check if the current color's king is in check (Unroll if the king is in check)
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
            buddy.reserve.Add(board(dsty, dstx))
            board(dsty, dstx) = " "c
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
      ElseIf pieceCaptured And saveCapture = "R"c Then
        Dim enemyQueenPiece = If(color = "w"c, "q"c, "Q"c)
        Dim enemyRank = If(color = "w"c, 0, 7)
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

      ' Then, check for both check and checkmate to get suffix.
CheckCheckAndMate:
      Dim suffix = ""
      Dim nchecks = 0
      Dim checkPiece = Convert.ToChar(0)
      testSquare = ""
      Dim attackerSquare = ""
      For Each fpiece In friendPieces
        testSquare = FindAttacker(fpiece, enemyKingSquare, "", True)
        If testSquare <> "" Then
          checkPiece = Char.ToUpper(fpiece)
          attackerSquare = testSquare
          nchecks += (attackerSquare.Length \ 2)
        End If
      Next

      If nchecks > 0 Then
        suffix = "+"
        Dim canEscape = False
        srcx = Convert.ToInt32(enemyKingSquare(0)) - Convert.ToInt32("a"c)
        srcy = Convert.ToInt32("8"c) - Convert.ToInt32(enemyKingSquare(1))
        For i = -1 To 1
          For j = -1 To 1
            If (srcx + j) >= 0 And (srcx + j) < 8 And (srcy + i) >= 0 And (srcy + i) < 8 Then
              testSquare = Convert.ToString(8 - srcy - i)
              testSquare = Convert.ToChar(Convert.ToInt32("a"c) + srcx + j) + testSquare
              Dim testPiece = board(srcy + i, srcx + j)
              Dim canEscapeToTest = Char.IsUpper(testPiece) <> Char.IsUpper(enemyKingPiece)
              canEscapeToTest = canEscapeToTest Or testPiece = " "c
              For Each fpiece In friendPieces
                canEscapeToTest = canEscapeToTest And FindAttacker(fpiece, testSquare, "", True) = ""
              Next
              canEscape = canEscape Or canEscapeToTest
            End If
          Next
        Next
        If Not canEscape Then
          Dim isMate = nchecks > 1
          dstx = Convert.ToInt32(attackerSquare(0)) - Convert.ToInt32("a"c)
          dsty = Convert.ToInt32("8"c) - Convert.ToInt32(attackerSquare(1))
          Dim magx = (dstx - srcx) * (dstx - srcx)
          Dim magy = (dsty - srcy) * (dsty - srcy)
          If Not isMate Then
            isMate = True
            For Each fpiece In friendPieces
              isMate = isMate And (FindAttacker(fpiece, attackerSquare, "", True) = "")
            Next
            isMate = isMate And (checkPiece = "N"c Or (magx <= 1 And magy <= 1))
            If isMate Then
              suffix = "#"
            End If
          End If
        End If
      End If
      Return suffix
    End Function
  End Class
End Namespace

