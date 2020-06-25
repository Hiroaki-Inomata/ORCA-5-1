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
      ************************************************************
      * ����饤����������˴ؤ�����������Ƴ���λ�����������å�
      * �ʥ쥻�ץ��ѡ�
      * COPY  (CPORCSRECESRCHK.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  05.00.02    NACL-ƣ��    20/04/08  ��Ǣ��Ω�ٱ�û���������Ǣ��
      *                                     Ω��Ƴ���λ�����������å��ɲ�
      ************************************************************
       01  ORCSRECESRCHKAREA.
      *������
      *    ������ʬ    ���򡧥���饤����������˴ؤ�뻻����������å�
      *                ��  ����Ǣ��Ω�˴ؤ�뻻����������å�
           03  LNK-RECESRCHK-SYORI         PIC X(01).
      *    ���ԣɣ�
           03  LNK-RECESRCHK-PTID          PIC 9(10).
      *    ����ǯ��ʿ���ǯ���
           03  LNK-RECESRCHK-SRYYM         PIC X(06).
      *    �����å���ʬ
           03  LNK-RECESRCHK-CHKKBN        PIC X(01).
      *    �����å��ֹ�
           03  LNK-RECESRCHK-CHKNO         PIC 9(02).
      *    ������ʬ
           03  LNK-RECESRCHK-NYUGAIKBN     PIC X(01).
      *    ������ʽ����������
           03  LNK-RECESRCHK-SRYYMD        PIC X(08)   OCCURS  2.
      *�ֵѹ���
           03  LNK-RECESRCHK-HENKYAKU-AREA.
      *�����������
      *  ľ�ỻ��ǯ��
               05  LNK-RECESRCHK-SANTEIYM  PIC X(06).
      *  ������ť�����
               05  LNK-RECESRCHK-SRYCD     PIC X(09).
      *  �������
               05  LNK-RECESRCHK-FSANTEIYMD
                                           PIC X(08).
      *  ����������
               05  LNK-RECESRCHK-MSANTEID  PIC X(02).
      *  ���������ȥ�����
               05  LNK-RECESRCHK-COMCD     PIC X(09).
      *
      *  �̻����
               05  LNK-RECESRCHK-KAISU     PIC 9(02).
      *
      *���������� ����̤���ꥨ�顼
      *           ��������ǯ��顼
      *           �������ԣɣĥ��顼
      *           ���������å���ʬ���顼
      *           ���������å��ֹ楨�顼
      *           ����������ʬ���顼
      *           ����������ʽ���������˥��顼
      *           ����������ʬ���顼
      *           ����������������Խ��ʤ�
      *           ����������������ʤ�
           03  LNK-RECESRCHK-RC            PIC S9(9) BINARY.
      *