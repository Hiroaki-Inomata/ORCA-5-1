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
      *  �v���O�����C������
      *  �쐬���t    ��Ǝ�       �L�q
      *  09/02/19    @@@@-�c��    �V�K�쐬
      ********************************************************************
       01  D5002.
           03  D5002-SYSYMD      PIC X(22).
           03  D5002-SRYYMD      PIC X(22).
           03  D5002-PAGE        PIC X(3).
           03  D5002-MIDASI      PIC X(10).
           03  D5002-JOBMSG      PIC X(60).
           03  D5002-NYUGAIKBN   PIC X(4).
           03  D5002-SRYKA       PIC X(20).
           03  D5002-TJIHI-G     OCCURS  10 TIMES.
               05  D5002-TJIHI     PIC X(8).
           03  D5002-HKNTEN      PIC X(09).
           03  D5002-SYUEKI      PIC X(10).
           03  D5002-MONEY-TBL   OCCURS  20 TIMES.
               05  D5002-MONEY     PIC X(08).
           03  D5002-SEIKYU      PIC X(10).
           03  D5002-RYOSYU      PIC X(10).