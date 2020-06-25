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
      *    �쥻�����¾���ǽ���ǡ��������ѥ�᥿������COPY (CPORCSBTUREC.INC)
      *                                ���ѥץ������ (ORCSBTUREC)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ����ǯ��
      *      �ݸ���ʬ
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSBTUREC-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-BTUREC-HOSPNUM                PIC  9(02).
      *���ԣɣ�
           03  LNK-BTUREC-PTID                   PIC  9(10).
      *����ǯ��
           03  LNK-BTUREC-SANTEIYM               PIC  X(06).
      *�ֵѹ���
           03  LNK-BTUREC-HENKYAKU-AREA.
      *��������ֵѥơ��֥�
             04    LNK-BTUREC-HENKYAKU-TBL       OCCURS  31.
      *��������ݸ��ȹ礻
                05  LNK-BTUREC-HKNCOMBI          PIC  9(04).
      *���ż���
                05  LNK-BTUREC-SRYSKB            PIC  X(02).
      *���拾���ɡʿ��Ź԰٥����ɡ�
                05  LNK-BTUREC-BTUCD             PIC  X(09).
      *�����ݸ��ȹ礻�ֵѥơ��֥����ٿ�
             04    LNK-BTUREC-SYU-HKNCOMBI-MEISAI
                                                 PIC  9(02).
      *�����ݸ��ȹ礻�ֵѥơ��֥�
             04    LNK-BTUREC-SYU-HKNCOMBI-TBL   OCCURS  31.
      *�����ݸ��ȹ礻
                05  LNK-BTUREC-SYU-HKNCOMBI      PIC  9(04)  OCCURS 31.
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼
      *            ����������פʤ����������ݸ��ȹ礻�������顼
      *            �������ŵ��ؼ��̥��顼��
           03  LNK-BTUREC-RC                     PIC S9(9) BINARY.
      *