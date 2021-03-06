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
      *    ���Ų���������������֡������ѥ�᥿��COPY (CPORCSTOTALACCT.INC)
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-ƣ��    11/09/16  ����ñ���б�
      *                                     �������磵�����ѹ���ʻ����
      *                                     �ݸ��ȹ礻���ѹ�
      ************************************************************
       01  ORCSTOTALACCT-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-ORCSTOTALACCT-HOSPNUM               PIC 9(2).
      *������ʬ
           03  LNK-ORCSTOTALACCT-NYUGAIKBN             PIC  X(01).
      *���ԣɣ�
           03  LNK-ORCSTOTALACCT-PTID                  PIC  9(10).
      *����ǯ��
           03  LNK-ORCSTOTALACCT-SANTEIYM              PIC  X(06).
      *������ʬ�ʶ������������ǲ���ֵѡ����������ֵѡ��������ǲ���ֵѡ�
      *        ���������λ��Τ߽�����ʬͭ����
           03  LNK-ORCSTOTALACCT-SHORIKBN              PIC  X(01).
      *
      *�ֵѹ���
      *
           03  LNK-ORCSTOTALACCT-OUT-AREA.
      *
               05  LNK-ORCSTOTALACCT-OUT-TBL           OCCURS  50.
      *�ݸ��ȹ礻�ֹ�
                   07  LNK-ORCSTOTALACCT-HKNCOMBINUM   PIC 9(04).
      *���Ų��ֵѥơ��֥�
                   07  LNK-ORCSTOTALACCT-OUT-TBL2      OCCURS  50.
      *���Ų�
                       09  LNK-ORCSTOTALACCT-SRYKA     PIC  X(02).
      *����
                       09  LNK-ORCSTOTALACCT-TENSU     PIC S9(08).
      *���ǲ��
                       09  LNK-ORCSTOTALACCT-KAISU     PIC  9(03).
      *
      *����������
      *
           03  LNK-ORCSTOTALACCT-RC                    PIC S9(9) BINARY.
      *
