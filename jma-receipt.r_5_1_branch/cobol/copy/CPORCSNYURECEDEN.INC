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
      *    �쥻����������׼����ѥ�᥿������COPY (CPORCSNYURECEDEN.INC)
      *                                ���ѥץ������ (ORCSNYURECEDEN)
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
       01  ORCSNYURECEDEN-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-NYURECEDEN-HOSPNUM                PIC  9(02).
      *���ԣɣ�
           03  LNK-NYURECEDEN-PTID                   PIC  9(10).
      *����ǯ��
           03  LNK-NYURECEDEN-SANTEIYM               PIC  X(06).
      *�ݸ���ʬ�ʣ������ݡ��������ݡ���������Ϣ���
           03  LNK-NYURECEDEN-HKNKBN                 PIC  X(01).
      *�о��ݸ��ȹ礻�ֹ�
           03  LNK-NYURECEDEN-SET-HKNCOMBI           PIC  9(04)
                                                          OCCURS 10.
      *�ֵѹ���
           03  LNK-NYURECEDEN-HENKYAKU-AREA.
      *�����ֵѥơ��֥��ź��������������ź��������������������������
      *                  ź�������������ܡʿ������ܡ�
      *                  ź�������������ܡʴĶ����ܡˡ�
             04  LNK-NYURECEDEN-HENKYAKU-TABLE       OCCURS  5.
      *�������ֵѥơ��֥�
               05  LNK-NYURECEDEN-HENKYAKU-TBL       OCCURS  31.
      *�ݸ��ȹ礻
                   07  LNK-NYURECEDEN-HKNCOMBI       PIC  9(04).
      *���ٿ�
                   07  LNK-NYURECEDEN-MEISAISU       PIC  9(03).
      *���̾���
                   07  LNK-NYURECEDEN-DAY-TBL.
                       09  LNK-NYURECEDEN-DAY        PIC  9(01)
                                                          OCCURS 31.
      *�쥻�ŵ�Ͽ���ꥢ
                   07  LNK-NYURECEDEN-NYUIN          OCCURS 300.
      *�쥳���ɼ���
                       09  LNK-NYURECEDEN-RECSKB     PIC  X(02).
      *���ż���
                       09  LNK-NYURECEDEN-SRYSKB     PIC  X(02).
      *���Ź԰٥�����
                       09  LNK-NYURECEDEN-SRYCD      PIC  X(09).
      *����
                       09  LNK-NYURECEDEN-SURYO      PIC  9(01).
      *����
                       09  LNK-NYURECEDEN-TEN        PIC  9(07).
      *���
                       09  LNK-NYURECEDEN-KAISU      PIC  9(02).
      *�����ȵ�Ͽ�ǡ���
                       09  LNK-NYURECEDEN-COMMENT    PIC  X(76).
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼�������ݸ���ʬ���顼
      *            ����������פʤ����������ݸ��ȹ礻�������顼��
           03  LNK-NYURECEDEN-RC                     PIC S9(9) BINARY.
      *
