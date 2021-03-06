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
      *----------------------------------------------------------*
      *    ��������¿����Ϳ���֥롼����ѥ�᥿
      *                                   COPY (CPORCSPSYCHO.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSPSYCHOAREA.
      *    ��������
           03  ORCSPSYCHO-SRYYMD       PIC X(08).
      *    �����ʥơ��֥�
           03  ORCSPSYCHO-MEDICINE-TBL.
               05  ORCSPSYCHO-MEDICINE-OCC     OCCURS   50.
      *            ���ޥ�����
                   07  ORCSPSYCHO-SRYCD        PIC X(09).
      *            ������ॳ����
                   07  ORCSPSYCHO-YAKKAKJNCD   PIC X(12).
      *            ���ż���
                   07  ORCSPSYCHO-SRYSYUKBN    PIC X(03).
      *            ���
                   07  ORCSPSYCHO-KAISU        PIC 9(03).
      *    �ֵ�
           03  ORCSPSYCHO-OUT-AREA.
      *        ��������ֵ�
               05  ORCSPSYCHO-MED-OUT-OCC      OCCURS   50.
      *            ���ޤθ�ǽ��ʬ
                   07  ORCSPSYCHO-FFECT-KBN    PIC 9(01).
      *            �����ʬ
                   07  ORCSPSYCHO-GROUP-KBN    PIC X(07).
      *            �׻�������ʬ
                   07  ORCSPSYCHO-RINJI-KBN    PIC X(01).
      *        ��������ͭ
               05  ORCSPSYCHO-FFECT-FLG    PIC 9(01).
      *        ¿����Ϳ��̡ʸ�ǽ�����
      *          ��1:���԰�����2:��̲����3:����������4:������������
               05  ORCSPSYCHO-COUNT        PIC 9(02)   OCCURS   4.
      *        ¿����Ϳ̵ͭ��1:¿����Ϳ�˳�������)
               05  ORCSPSYCHO-TAZAI        PIC 9(01).
      *
