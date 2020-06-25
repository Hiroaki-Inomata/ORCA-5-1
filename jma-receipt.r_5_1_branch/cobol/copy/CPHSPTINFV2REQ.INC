      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    �`�o�h���N�G�X�g���@���Ҋ�{��� 
      *    COPY (CPHSPTINFV2REQ.INC)
      ******************************************************************
        01  XML-HSPTINFV2REQ.
            03  HSPTINFREQ-CONTEXT             PIC S9(9)   BINARY.
            03  HSPTINFREQ-OBJECT              PIC X(36).
            03  HSPTINFREQ-MODE                PIC S9(9)   BINARY.
            03  HSPTINFREQ-RECORDNAME          PIC X(128).
      *
            03  HSPTINFREQ-PRIVATE-OBJECTS.
                05  HSPTINFREQ-PTNUM           PIC X(20).
                05  HSPTINFREQ-NYUINYMD        PIC X(10).