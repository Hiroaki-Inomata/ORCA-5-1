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
      *    ����ƣ������������׼����ѥ�᥿��COPY (CPORCHCN33SUB.INC)
      *                                ���ѥץ������ (ORCHCN33SUB.CBL)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ����ǯ��
      *      ���Ų�
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCHCN33SUB-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-ORCHCN33SUB-HOSPNUM               PIC 9(2).
      *���ԣɣ�
           03  LNK-ORCHCN33SUB-PTID                  PIC  9(10).
      *����ǯ��
           03  LNK-ORCHCN33SUB-SANTEIYM              PIC  X(06).
      *���Ųʡʣ��������ʡ�
           03  LNK-ORCHCN33SUB-SRYKA                 PIC  X(02).
      *������ʬ�ʶ��򡧥���ƣ�����ѡ��������Ų������������ֵѡ�
      *          �������Ų������������ֵѡ�
           03  LNK-ORCHCN33SUB-SHORIKBN              PIC  X(01).
      *�ֵѹ���
           03  LNK-ORCHCN33SUB-HENKYAKU-AREA.
      *����ƣ�����������ֵѥơ��֥�
             04  LNK-ORCHCN33SUB-HENKYAKU-TABLE.
      *�������ֵѥơ��֥�
               05  LNK-ORCHCN33SUB-HENKYAKU-TBL      OCCURS  31.
      *��������
                   07  LNK-ORCHCN33SUB-TEN           PIC S9(07).
      *����¾����������ô��������
                   07  LNK-ORCHCN33SUB-ETCTEN        PIC  9(07).
      *��������
                   07  LNK-ORCHCN33SUB-SKJ           PIC  9(07).
      *���Ų������������ֵѥơ��֥�
             04  LNK-ORCHCN33SUB-HENKYAKU-TABLE2.
      *�ݸ��ȹ礻�ֵѥơ��֥�
               05  LNK-ORCHCN33SUB-HENKYAKU-TBL2     OCCURS  10.
      *�ݸ��ȹ礻�ֹ�
                   07  LNK-ORCHCN33SUB-HENKYAKU-HKNCOM  PIC 9(04).
      *���Ų��ֵѥơ��֥�
                   07  LNK-ORCHCN33SUB-HENKYAKU-TBL3     OCCURS  50.
      *���Ų�
                       09  LNK-ORCHCN33SUB-NYUINKA   PIC  X(02).
      *������
                       09  LNK-ORCHCN33SUB-STDAY     PIC  9(02).
      *��λ��
                       09  LNK-ORCHCN33SUB-EDDAY     PIC  9(02).
      *����
                       09  LNK-ORCHCN33SUB-TENSU     PIC S9(07).
      *����
                       09  LNK-ORCHCN33SUB-DAY       PIC  9(02).
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼���������Ųʥ��顼
      *            ����������פʤ�����������������ʤ���
           03  LNK-ORCHCN33SUB-RC                    PIC S9(9) BINARY.
      *
