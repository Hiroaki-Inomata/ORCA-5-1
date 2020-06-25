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
      *    ��ե����ɼ����ѥѥ�᥿      COPY (CPORCSDRCDGET.INC)
      *                                  ���ѥץ������ (ORCSDRCDGET)
      *    �������
      *      ����ǯ��
      *      ������ʬ
      *      ���ԣɣ�
      *      �ݸ��ȹ礻
      *     �������Τߡ�
      *      �о�ǯ��������
      *      ����ǯ����
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
           03  LNK-DRCDGET-IN.
      *        ����ǯ��
               05  LNK-DRCDGET-I-SRYYM     PIC 9(06).
      *        ������ʬ
               05  LNK-DRCDGET-I-NYUGAIKBN PIC X(01).
      *        ���ԣɣ�
               05  LNK-DRCDGET-I-PTID      PIC 9(10).
      *        �ݸ��ȹ礻����
               05  LNK-DRCDGET-I-HKNCOMBI-OCC.
      *        �ݸ��ȹ礻
                   07  LNK-DRCDGET-I-HKNCOMBI
                                           PIC 9(04)   OCCURS  10.
      *        �о�ǯ��������
               05  LNK-DRCDGET-I-TEKYMD.
      *        Ŭ�ѳ���ǯ����
                   07  LNK-DRCDGET-I-TEKSTYMD
                                           PIC X(08). 
      *        Ŭ�ѽ�λǯ����
                   07  LNK-DRCDGET-I-TEKEDYMD
                                           PIC X(08). 
      *        ����ǯ����
               05  LNK-DRCDGET-I-NYUINYMD  PIC X(08). 
      *
           03  LNK-DRCDGET-OT.
      *        ��ե�����
               05  LNK-DRCDGET-O-DRCD      PIC X(05).
      *        ���Ų�
               05  LNK-DRCDGET-O-DRCD-SRYKA
                                           PIC X(02).
      *    ����������
           03  LNK-DRCDGET-RC           PIC S9(9) BINARY.