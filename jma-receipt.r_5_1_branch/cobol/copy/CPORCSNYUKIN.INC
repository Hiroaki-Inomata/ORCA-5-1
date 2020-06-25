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
      *    �ޤȤ������Խ��ѥ�᥿                                 *
      *                         COPY  (CPORCSNYUKIN.INC)          *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSNYUKINAREA.
      *������
      *    ���ŵ��أɣġ�ɬ�ܡ�
           03  LNK-SCNYUKIN-HOSPNUM          PIC 9(2).
      *    ���ԣɣġ�ɬ�ܡ�
           03  LNK-SCNYUKIN-PTID             PIC 9(10).
      *    ������ʬ
      *      ��������
      *      ��������
           03  LNK-SCNYUKIN-NYUGAIKBN        PIC X(01).
      *    ������ʬ��ɬ�ܡ�
      *      ���������ǧ���̤�����
      *      ����̤���ޤȤ�������̤�����
           03  LNK-SCNYUKIN-SYORIKBN         PIC X(01).
      *    ������ʬ��ɬ�ܡ�
      *      ����̤���������ʼ�Ǽ�¹����ʤ���
      *      ����̤������
           03  LNK-SCNYUKIN-KOUSHINKBN       PIC X(01).
      *    �ֶ��ʬ
      *      �����ֶ�ۤ���ν����򤷤ʤ�
           03  LNK-SCNYUKIN-HENKINKBN        PIC X(01).
      *    ���������ʬ
           03  LNK-SCNYUKIN-NYKNKNRKBN       PIC X(01).
      *    �����
           03  LNK-SCNYUKIN-NYUKIN-MONEY     PIC 9(08).
      *    �ֶ��
           03  LNK-SCNYUKIN-HENKIN-MONEY     PIC 9(08).
      *    ������
           03  LNK-SCNYUKIN-NYUKIN-YMD       PIC X(08).
      *    ������ˡ
           03  LNK-SCNYUKIN-NYUKIN-HOHO      PIC X(02).
      *������
           03  LNK-SCNYUKIN-OUT-AREA.
      *
      *        �ޤȤ������ʬ
               05  LNK-SCNYUKIN-MATOME-NYUHENKIN-KBN.
      *            ��������ʤ�1:���⤢��
                   07  LNK-SCNYUKIN-MATOME-NYUKIN-KBN  PIC X(01).
      *            �����ֶ�ʤ�1:�ֶ⤢��
                   07  LNK-SCNYUKIN-MATOME-HENKIN-KBN  PIC X(01).
      *        ���⿶��ʬ������
               05  LNK-SCNYUKIN-FURIWAKE-INF   OCCURS  100.
      *            ��ɼ�ֹ�
                   07  LNK-SCNYUKIN-DENPNUM    PIC 9(07).
      *            �����
                   07  LNK-SCNYUKIN-SKYMONEY   PIC 9(07).
      *            ���⿶��ʬ����
                   07  LNK-SCNYUKIN-FURIWAKEMONEY  PIC 9(07)
                                                   OCCURS 2.
      *        �ֶ⿶��ʬ���ĳ�
               05  LNK-SCNYUKIN-HENKINZAN      PIC 9(07).
      *        ���⿶��ʬ���ĳ�
               05  LNK-SCNYUKIN-FURIWAKEZAN    PIC 9(07).
      *����������
      *    00:����
      *    10:�ѥ�᡼������
      *    11:���⿶��ʬ���оݥǡ����ʤ�
      *    12:���⿶��ʬ���оݥǡ�����������ʾ�ΰ١������Բ�
      *    13:��Ǽ�������顼
      *    14:��Ǽ���٥��󥵡��ȥ��顼
           03  LNK-SCNYUKIN-RC              PIC 9(02).
      *
      *