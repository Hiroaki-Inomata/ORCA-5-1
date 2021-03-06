      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *----------------------------------------------------------*
      *    ���Ź԰������������þ�������å��ѥ�᥿
      *                                          COPY (CPORCSC98.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSC98AREA.
      *    �У�̾
           03  LNK-SC98-PG             PIC X(06).
      *    ������ʬ�ʣ�������Ϳ�̡���������å���
           03  LNK-SC98-KBN            PIC X(01).
      *
           03  LNK-SC98-INAREA.
      *        ǯ�� 
               05  LNK-SC98-NENREI.
                   07  LNK-SC98-NENREI-YY     PIC 9(03).
                   07  LNK-SC98-NENREI-MM     PIC 9(02).
                   07  LNK-SC98-NENREI-DD     PIC 9(02).
      *H28.11
      *    ������Ϳ�̥����å���ʬ
           03  LNK-SC98-TOUYOCHK-FLG           PIC X(01).
      *
      *    ����Ϳ���̡�����ơ��֥�
           03  LNK-SC98-YAKUZAI-AREA.
               05  LNK-SC98-YAKUZAI-TBL             OCCURS   100.
                   07  LNK-SC98-YAKUZAI-CHKFLG      PIC 9(01).
                   07  LNK-SC98-YAKUZAI-SRYCD       PIC X(09).
                   07  LNK-SC98-YAKUZAI-SURYO       PIC 9(05)V9(05).
                   07  LNK-SC98-YAKUZAI-KAISU       PIC 9(03).
                   07  LNK-SC98-YAKUTEI-SURYO       PIC S9(05)V9(05).
                   07  LNK-SC98-YAKUTEI-KAISU       PIC S9(03).
      *
           03  LNK-SC98-OUTAREA.
      *        ���顼����
               05  LNK-SC98-ERRMSG         PIC X(100).
      *
