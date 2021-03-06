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
      *    ������������������ô�ۼ����ѥ�᥿����COPY (CPORCSNYULIFE.INC)
      *                                ���ѥץ������ (ORCSNYULIFE)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ����ǯ��
      *
      *    ���ֵѹ��ܤο����˷����ۤϣ���ʬ���ͤ��ֵ�
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSNYULIFE-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-NYULIFE-HOSPNUM                    PIC 9(2).
      *���ԣɣ�
           03  LNK-NYULIFE-PTID                       PIC  9(10).
      *����ǯ��
           03  LNK-NYULIFE-SANTEIYM                   PIC  X(06).
      *�ֵѹ���
           03  LNK-NYULIFE-HENKYAKU-AREA.
      *�����������������ֵѥơ��֥�
               05  LNK-NYULIFE-HENKYAKU-TABLE         OCCURS  31.
      *�����������������оݶ�ʬ
      *  �ʶ����оݳ��������оݡ�
                   07  LNK-NYULIFE-KBN                PIC  X(01).
      *�����������������ʬ
      *  �ʣ������������񣱡��������������񣲡�
                   07  LNK-NYULIFE-SKJKBN             PIC  X(01).
      *������������������ô�ڸ���ʬ���ʽ����ξ������ڸ���
      *  �ʶ��򡧷ڸ�̵�����������������������������ݣ�������
      *    ��������������������������Ϸ��ʡ��ǯ�����ԡ�
                   07  LNK-NYULIFE-FTNKBN1            PIC  X(01).
      *������������������ô�ڸ���ʬ�����¾������������ڸ���
      *  �ʶ��򡧷ڸ�̵���������ڸ�ͭ���
                   07  LNK-NYULIFE-FTNKBN2            PIC  X(01).
      *����������������ݿ����ݿ��ť�����
                   07  LNK-NYULIFE-SKJ-SRYCD          PIC  X(09).
      *����������������ݿ����ݶ��
                   07  LNK-NYULIFE-SKJ-MONEY          PIC  9(07).
      *����������������ݴĶ����ܡݿ��ť�����
                   07  LNK-NYULIFE-KAN-SRYCD          PIC  X(09).
      *����������������ݴĶ����ܡݶ��
                   07  LNK-NYULIFE-KAN-MONEY          PIC  9(07).
      *����������������ݿ�����ô�ݿ��ť�����
                   07  LNK-NYULIFE-SKJFTN-SRYCD       PIC  X(09).
      *����������������ݿ�����ô�ݶ��
                   07  LNK-NYULIFE-SKJFTN-MONEY       PIC  9(07).
      *����������������ݴĶ�������ô�ݿ��ť�����
                   07  LNK-NYULIFE-KANFTN-SRYCD       PIC  X(09).
      *����������������ݴĶ�������ô�ݶ��
                   07  LNK-NYULIFE-KANFTN-MONEY       PIC  9(07).
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼
      *            ����������פʤ����������������������оݳ���
           03  LNK-NYULIFE-RC                     PIC S9(9) BINARY.
      *
