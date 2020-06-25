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
      *    ϫ��������׺����ѥѥ�᥿������COPY (CPORCSROUNYUIN.INC)
      *                                ���ѥץ������ (ORCSROUNYUIN)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ����ǯ��
      *      �ݸ��ȹ礻
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.05.01     NACL-����    10/01/18  ��������μ�������ʬ�ɲ�
      * 04.05.02     NACL-����    12/04/09  ����ά�Τ�ʸ������ĥ
      *
      * 04.07.01     NACL-����    11/11/22  Ʊ���������б�
      * 04.08.01     NACL-����    18/02/01  �軰�Թ԰٤������������б�
      * 04.08.02     NACL-����    18/03/01  ʿ������ǯ��������б�
      ************************************************************
       01  ORCSROUNYUIN-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-SROUNYUIN-HOSPNUM                 PIC 9(2).
      *���ԣɣ�
           03  LNK-SROUNYUIN-PTID                    PIC  9(10).
      *����ǯ��
           03  LNK-SROUNYUIN-SANTEIYM                PIC  X(06).
      *�ݸ��ȹ礻
           03  LNK-SROUNYUIN-HKNCOMBI                PIC  9(04).
      *ά���Խ���ʬ�ʶ��򡧤��ʤ������������
           03  LNK-SROUNYUIN-RYAKUKBN                PIC  X(01).
      *ϫ�Ҿ����ʬ�ʣ��������ְ��⡡���������ֱۤ���
           03  LNK-SROUNYUIN-JOUSU-KBN               PIC  X(01)
                                                     OCCURS 31.
      *�軰�Թ԰٥ե饰�ʼ����ա��軰�Թ԰١˥쥻�ץ��ѡ�
           03  LNK-SROUNYUIN-DAISANFLG               PIC  X(01).
      *�ֵѹ���
           03  LNK-SROUNYUIN-HENKYAKU-AREA.
      *�ֵѥơ��֥�
               05  LNK-SROUNYUIN-HENKYAKU-TBL        OCCURS  31.
      *ϫ�������ե饰�ʶ���ϫ�������ʤ�������ϫ���������
                   07  LNK-SROUNYUIN-NYUIN-FLG       PIC  X(01).
      *�����ʬ
                   07  LNK-SROUNYUIN-GAIHAKU         PIC  9(03).
      *�������������Ŷ�ʬ
                   07  LNK-SROUNYUIN-SRYKBN          PIC  X(02).
      *������
                   07  LNK-SROUNYUIN-ZAITEN          PIC S9(08).
      *���ť����ɷ�
                   07  LNK-SROUNYUIN-SRYCDTOTAL      PIC  9(14).
      *���ٿ�
                   07  LNK-SROUNYUIN-MEISAISU        PIC  9(07).
      *���ť�����
                   07  LNK-SROUNYUIN-ROUSAI          OCCURS  20.
                       09  LNK-SROUNYUIN-SRYCD       PIC  X(09).
                       09  LNK-SROUNYUIN-SRYCD-SK    PIC  X(01).
      *ά�Ρ�������������
                   07  LNK-SROUNYUIN-RYAKUSYO        PIC  X(20).
      *������׺޼��̶�ʬ���κ�����
                   07  LNK-SROUNYUIN-ZAITEN-NACCT    PIC  9(08).
      *���Ų�ף����������κ�����
                   07  LNK-SROUNYUIN-ZAITEN-ACCT     PIC  9(08).
      *���Ų�ף����������κ������ʣ���
                   07  LNK-SROUNYUIN-ZAITEN-ACCT2    PIC  9(08).
      *���Ų�ף����������κ������ʣ���
                   07  LNK-SROUNYUIN-ZAITEN-ACCT3    PIC  9(08).
      *���Ų�ף����������κ������ʤ���¾��
                   07  LNK-SROUNYUIN-ZAITEN-ACCTETC  PIC  9(08).
      *���Ų�ף����������κ������ʤ���¾�ˡʣ���
                   07  LNK-SROUNYUIN-ZAITEN-ACCTETC2 PIC  9(08).
      *���Ų�ף����������κ������ʤ���¾�ˡʣ���
                   07  LNK-SROUNYUIN-ZAITEN-ACCTETC3 PIC  9(08).
      *��������μ�������ʬ
                   07  LNK-SROUNYUIN-GENTEN          PIC  9(08).
      *�����������ե饰
                   07  LNK-SROUNYUIN-SENTEI-FLG      PIC  X(01).
      *
      *ά�Ρʰ������������û���
               05  LNK-SROUNYUIN-RYAKUSYO2           PIC  X(20)
                                                     OCCURS 20.
      *
      *���û��������������û�
               05  LNK-SROUNYUIN-CHUKSNSRYCD         PIC  X(09)
                                                     OCCURS 100.
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼������ϫ�������ʤ���
           03  LNK-SROUNYUIN-RC                      PIC S9(9) BINARY.
      *