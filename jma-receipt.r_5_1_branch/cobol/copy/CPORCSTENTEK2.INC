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
      ************************************************************
      *    ������ũ��Хѥ�᥿����COPY (CPORCSCTENTEK2.INC)
      *                            ���ѥץ������ 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSTENTEK2AREA.
      *
      *���ŵ��أɣ�
           03  LNK-ORCSTEN2-HOSPNUM            PIC  9(02).
      *���ԣɣ�
           03  LNK-ORCSTEN2-PTID               PIC  9(10).
      *����ǯ��
           03  LNK-ORCSTEN2-SRYYM              PIC  X(06).
      *�ݸ��ȹ�
           03  LNK-ORCSTEN2-HKNCOMBI           PIC 9(04).
      *�ֵѹ���
           03  LNK-ORCSTEN2-OUT-AREA.
      *        ���ֹ�
               05  LNK-ORCSTEN2-ZAINUM         PIC 9(08)  OCCURS 40.
      *        ��ũ����
               05  LNK-ORCSTEN2-TENTEK-AREA.
                   07  LNK-ORCSTEN2-TENTEK-DAY            OCCURS   31.
      *              ���Ų�
                     09  LNK-ORCSTEN2-TENTEK-SRYKA        PIC X(02).
      *              ���ٿ�
                     09  LNK-ORCSTEN2-TENTEK-MEISAISU     PIC 9(03).
      *              ������
                     09  LNK-ORCSTEN2-TENTEK-ZAITEN       PIC 9(07).
      *              ���ť����ɷ�
                     09  LNK-ORCSTEN2-TENTEK-SRYCDTOTAL   PIC 9(13).
      *              �����ֵ�
                     09  LNK-ORCSTEN2-TENTEK-MEISAI       OCCURS  5.
      *                ���Ź԰٥�����
                       11  LNK-ORCSTEN2-TENTEK-SRYCD      PIC X(09).
      *                ���������ٿ�
                       11   LNK-ORCSTEN2-TENTEK-COM-MEISAISU
                                                          PIC 9(03).
      *                �������ֵ��ΰ�
                       11  LNK-ORCSTEN2-TENTEK-COM-AREA   OCCURS   5.
      *                 �����ȥե饰�ʶ����̾拾���ȡ�������­�����ȡ�
                        13  LNK-ORCSTEN2-TENTEK-COM-FLG   PIC X(1).
      *                 �����ȥ�����
                        13  LNK-ORCSTEN2-TENTEK-COM-SRYCD PIC X(09).
      *                 ������ʸ��
                        13  LNK-ORCSTEN2-TENTEK-COM-MOJI  PIC X(100).
      *        ���顼�����ɡʣ������ｪλ�������оݤʤ���
                  07  LNK-ORCSTEN2-TENTEK-ERRCD           PIC 9(01).
      *
      *        �濴��̮
               05  LNK-ORCSTEN2-CHUJOU-AREA.
                   07  LNK-ORCSTEN2-CHUJOU-DAY            OCCURS   31.
      *              ���Ų�
                     09  LNK-ORCSTEN2-CHUJOU-SRYKA        PIC X(02).
      *              ���ٿ�
                     09  LNK-ORCSTEN2-CHUJOU-MEISAISU     PIC 9(03).
      *              ������
                     09  LNK-ORCSTEN2-CHUJOU-ZAITEN       PIC 9(07).
      *              ���ť����ɷ�
                     09  LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL   PIC 9(13).
      *              �����ֵ�
                     09  LNK-ORCSTEN2-CHUJOU-MEISAI       OCCURS  5.
      *                ���Ź԰٥�����
                       11  LNK-ORCSTEN2-CHUJOU-SRYCD      PIC X(09).
      *                ���������ٿ�
                       11   LNK-ORCSTEN2-CHUJOU-COM-MEISAISU
                                                          PIC 9(03).
      *                �������ֵ��ΰ�
                       11  LNK-ORCSTEN2-CHUJOU-COM-AREA   OCCURS   5.
      *                 �����ȥե饰�ʶ����̾拾���ȡ�������­�����ȡ�
                        13  LNK-ORCSTEN2-CHUJOU-COM-FLG   PIC X(1).
      *                 �����ȥ�����
                        13  LNK-ORCSTEN2-CHUJOU-COM-SRYCD PIC X(09).
      *                 ������ʸ��
                        13  LNK-ORCSTEN2-CHUJOU-COM-MOJI  PIC X(100).
      *        ���顼�����ɡʣ������ｪλ�������оݤʤ���
                  07  LNK-ORCSTEN2-CHUJOU-ERRCD           PIC 9(01).
      *      ���������ɡʣ�������ǯ��顼���������Ը������顼
      *                  �����ݸ��ȹ礻���顼������������פʤ�
      *                  �����굻�ܲû��ȹ礻���顼��
             03  LNK-ORCSTEN2-RC              PIC S9(9)  BINARY.
      *