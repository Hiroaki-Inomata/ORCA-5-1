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
      *         �����å��㳰�ޥ���        COPY (CPCHKREIGAI.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  CHKREIGAI-REC.
      *�����å��ֹ�
           03  CHKREIGAI-KEY.
               05  CHKREIGAI-HOSPNUM       PIC 9(02).
      *���ԣɣ�
               05  CHKREIGAI-PTID          PIC 9(10).
      *Ϣ��
               05  CHKREIGAI-RENNUM        PIC 9(08).
      *��ǧ�����ֹ�
      *    01:�ݸ����ֹ�
      *    02:���ݸ��Ե��桦�ֹ�
      *    03:������ô���ֹ桦������ֹ�
      *    04:�ݸ�Ŭ��ǯ��
      *    05:������̾
      *    06:���ų�����
      *    07:������̾��������̾
      *    08:���ޤ���̾
      *    09:���Ź԰٤���̾
      *    10:���ޤ�ʻ�Ѷش�
      *    11:���Ź԰٤�ʻ�ѻ���
      *    12:������
      *    13:���Ի�̾
      *    14:�ݸ��ȹ礻ͭ������
      *    15:��Ϳ�ش����ޤ���̾
      *    16:���Ź԰٤ɤ����λ���ϳ��
      *    17:��̾������
      *    18:��̾�ȿ��Ź԰�
      *    19:���ޤη��²��������Ϳ��
      *    20:���Ź԰ٻ�����
      *    21:���򼰥�����
      *    99:����¾
      *    A1:̤��Ͽ���ޡ����ޤ���̾��
           03  CHKREIGAI-CHKNUM            PIC X(02).
      *������ʬ
           03  CHKREIGAI-NYUGAIKBN         PIC X(01).
      *�����å���ʬ
           03  CHKREIGAI-CHKKBN            PIC X(01).
      *���ť�����
           03  CHKREIGAI-SRYCD             PIC X(09).
      *��̾
           03  CHKREIGAI-BYOMEI            PIC X(160).
      *����ʬ
           03  CHKREIGAI-COMPKBN           PIC X(1).
      *Ŭ�ѥ�����
           03  CHKREIGAI-TEKCD             PIC X(9).
      *Ŭ����̾
           03  CHKREIGAI-TEKBYOMEI         PIC X(160).
      *Ŭ����̾����ʬ
           03  CHKREIGAI-TEKCOMPKBN        PIC X(1).
      *ñ�̥�����
           03  CHKREIGAI-TANICD            PIC 9(3).
      *ñ��̾��
           03  CHKREIGAI-TANINAME          PIC X(12).
      *���顼������
           03  CHKREIGAI-ERRCD             PIC X(06).
      *���顼��å�����
           03  CHKREIGAI-ERRMSG            PIC X(200).
      *����ͭ��ǯ����
           03  CHKREIGAI-STYUKYMD          PIC X(08).
      *��λͭ��ǯ����
           03  CHKREIGAI-EDYUKYMD          PIC X(08).
      *ü���ɣ�
           03  CHKREIGAI-TERMID            PIC X(16).
      *���ڡ��졼���ɣ�
           03  CHKREIGAI-OPID              PIC X(16).
      *����ǯ����
           03  CHKREIGAI-CREYMD            PIC X(08).
      *����ǯ����
           03  CHKREIGAI-UPYMD             PIC X(08).
      *��������
           03  CHKREIGAI-UPHMS             PIC X(06).
           03  CHKREIGAI-SELLIMIT          PIC 9(08).
           03  CHKREIGAI-SELOFFSET         PIC 9(08).
