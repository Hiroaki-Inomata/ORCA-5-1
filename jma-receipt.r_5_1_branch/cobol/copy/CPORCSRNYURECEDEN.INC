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
      *    ϫ�ҥ쥻����������׼����ѥ�᥿������COPY (CPORCSRNYURECEDEN.INC)
      *                                ���ѥץ������ (ORCSRNYURECEDEN)
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
       01  ORCSRNYURECEDEN-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-RNYURECEDEN-HOSPNUM               PIC  9(02).
      *���ԣɣ�
           03  LNK-RNYURECEDEN-PTID                  PIC  9(10).
      *����ǯ��
           03  LNK-RNYURECEDEN-SANTEIYM              PIC  X(06).
      *�ݸ��ȹ礻�ֹ�
           03  LNK-RNYURECEDEN-HKNCOMBI              PIC  9(04).
      *�ݸ���ʬ�ʣ��򥻥åȡ�
           03  LNK-RNYURECEDEN-HKNKBN                PIC  X(01).
      *�ֵѹ���
           03  LNK-RNYURECEDEN-HENKYAKU-AREA.
      *�����ֵѥơ��֥��ź��������������ź����������������������������
             04  LNK-RNYURECEDEN-HENKYAKU-TABLE      OCCURS  3.
      *�������ֵѥơ��֥�
               05  LNK-RNYURECEDEN-HENKYAKU-TBL      OCCURS  31.
      *���ٿ�
                   07  LNK-RNYURECEDEN-MEISAISU      PIC  9(03).
      *���̾���
                   07  LNK-RNYURECEDEN-DAY-TBL.
                       09  LNK-RNYURECEDEN-DAY       PIC  9(01)
                                                          OCCURS 31.
      *�쥻�ŵ�Ͽ���ꥢ
                   07  LNK-RNYURECEDEN-NYUIN         OCCURS 300.
      *�쥳���ɼ���
                       09  LNK-RNYURECEDEN-RECSKB    PIC  X(02).
      *���ż���
                       09  LNK-RNYURECEDEN-SRYSKB    PIC  X(02).
      *���Ź԰٥�����
                       09  LNK-RNYURECEDEN-SRYCD     PIC  X(09).
      *����
                       09  LNK-RNYURECEDEN-SURYO     PIC  9(01).
      *�����ʿ����ξ��϶�ۤ��ֵѡ�
                       09  LNK-RNYURECEDEN-TEN       PIC  9(07).
      *���
                       09  LNK-RNYURECEDEN-KAISU     PIC  9(02).
      *�����ȵ�Ͽ�ǡ���
                       09  LNK-RNYURECEDEN-COMMENT   PIC  X(76).
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼��
      *            �����ݸ��ȹ礻�������顼������������פʤ���
           03  LNK-RNYURECEDEN-RC                     PIC S9(9) BINARY.
      *