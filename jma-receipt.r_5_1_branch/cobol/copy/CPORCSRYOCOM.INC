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
      *    ���ܥ쥻�����ȼ����ѥ�᥿������COPY (CPORCSRYOCOM.INC)
      *                                ���ѥץ������ (ORCSRYOCOM)
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
       01  ORCSRYOCOM-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-RYOCOM-HOSPNUM                PIC  9(02).
      *���ԣɣ�
           03  LNK-RYOCOM-PTID                   PIC  9(10).
      *����ǯ��
           03  LNK-RYOCOM-SANTEIYM               PIC  X(06).
      *�ֵѹ���
           03  LNK-RYOCOM-HENKYAKU-AREA.
      *�������ֵѥơ��֥�
             04  LNK-RYOCOM-HENKYAKU-TABLE       OCCURS  31.
      *����������ʬ�ʶ������������ʳ�����������������
               05  LNK-RYOCOM-NYUINKBN           PIC  X(01).
      *���ٿ�
               05  LNK-RYOCOM-MEISAISU           PIC  9(03).
      *���ť����ɷסʣ��ģ̥����ȴޤ��
               05  LNK-RYOCOM-SRYCDTOTAL         PIC  9(14).
      *���ť����ɷף��ʿ��ť����ɷסʣ��ģ̥����ȴޤ�ˡܣ��ģ�������
               05  LNK-RYOCOM-SRYCDTOTAL2        PIC  9(14).
      *�����ȵ�Ͽ���ꥢ
               05  LNK-RYOCOM-RECORD             OCCURS 30.
      *���Ŷ�ʬ�����ȥ�����
                   07  LNK-RYOCOM-IRYO-SRYCD     PIC  X(09).
      *���Ŷ�ʬ�����Ⱥ��ֹ�
                   07  LNK-RYOCOM-IRYO-ZAINUM    PIC  9(08).
      *���Ŷ�ʬ��������Ƭ��ʸ�����¤��ؤ��ѡ�
                   07  LNK-RYOCOM-IRYO-SORT-COM  PIC  X(04).
      *
      *���ģ̥����ȥ����ɡ�ʿ������ǯ����ޤǤ��ֵѹ��ܡ�
               05  LNK-RYOCOM-ADL-SRYCD          PIC  X(09).
      *���ģ������ʿ����ˡ�ʿ������ǯ����ޤǤ��ֵѹ��ܡ�
               05  LNK-RYOCOM-ADL-TEN            PIC  9(02).
      *���ģ����������ѡˡ�ʿ������ǯ����ޤǤ��ֵѹ��ܡ�
               05  LNK-RYOCOM-ADL-TEN-MOJI       PIC  X(04).
      *
      *���ģ��ֵ��ΰ��ʿ������ǯ����ʹߡ�
               05  LNK-RYOCOM-ADL-H22            OCCURS   4.
      *���ģ̥����ȥ�����
                 07  LNK-RYOCOM-ADL-SRYCD-H22    PIC  X(09).
      *���ģ������ʿ�����
                 07  LNK-RYOCOM-ADL-TEN-H22      PIC  9(02).
      *���ģ����������ѡ�
                 07  LNK-RYOCOM-ADL-TEN-MOJI-H22 PIC  X(04).
      *
      *���ģ��ֵ��ΰ��ʿ������ǯ����ʹߤϤ�����ˤ⿷�ߥ����ɤ��ֵѡ�
               05  LNK-RYOCOM-ADL-H28            OCCURS   4.
      *���ģ̥����ȥ�����
                 07  LNK-RYOCOM-ADL-SRYCD-H28    PIC  X(09).
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼
      *            ����������פʤ������������������ʤ�
      *            �������ܥ��������Ϥʤ���
           03  LNK-RYOCOM-RC                     PIC S9(9) BINARY.
      *