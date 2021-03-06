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
      *    ������������������ô�ۼ����ѥ�᥿����COPY (CPORCSNYULIFE2.INC)
      *                                ���ѥץ������ (ORCSNYULIFE2)
      *    ʿ������ǯ���������
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
       01  ORCSNYULIFE2-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-NYULIFE2-HOSPNUM                    PIC 9(2).
      *���ԣɣ�
           03  LNK-NYULIFE2-PTID                       PIC  9(10).
      *����ǯ��
           03  LNK-NYULIFE2-SANTEIYM                   PIC  X(06).
      *�ֵѹ���
           03  LNK-NYULIFE2-HENKYAKU-AREA.
      *�����������������ֵѥơ��֥�
               05  LNK-NYULIFE2-HENKYAKU-TABLE         OCCURS  31.
      *�����������������оݶ�ʬ
      *  �ʶ����оݳ��������оݡ�
                 06  LNK-NYULIFE2-KBN                  PIC  X(01).
      *�����������������ʬ
      *  �ʣ������������񣱡��������������񣲡�
                 06  LNK-NYULIFE2-SKJKBN               PIC  X(01).
      *������������������ô�ڸ���ʬ���ʽ����ξ������ڸ���
      *  �ʶ��򡧷ڸ�̵�����������������������������ݣ�������
      *    ��������������������������Ϸ��ʡ��ǯ�����ԡ�
                 06  LNK-NYULIFE2-FTNKBN1              PIC  X(01).
      *������������������ô�ڸ���ʬ�����¾������������ڸ���
      *  �ʶ��򡧷ڸ�̵���������ڸ�ͭ���
                 06  LNK-NYULIFE2-FTNKBN2              PIC  X(01).
      *�������֡�ź������ī����ź�������뿩��ź������ͼ����
                 06  LNK-NYULIFE2-HENKYAKU-TIME        OCCURS  3.
      *����������������ݿ����ݿ��ť�����
                   07  LNK-NYULIFE2-SKJ-SRYCD          PIC  X(09).
      *����������������ݿ����ݶ��
                   07  LNK-NYULIFE2-SKJ-MONEY          PIC  9(07).
      *����������������ݿ�����ô�ݿ��ť�����
                   07  LNK-NYULIFE2-SKJFTN-SRYCD       PIC  X(09).
      *����������������ݿ�����ô�ݶ��
                   07  LNK-NYULIFE2-SKJFTN-MONEY       PIC  9(07).
      *����������������ݴĶ����ܡݿ��ť�����
                 06  LNK-NYULIFE2-KAN-SRYCD            PIC  X(09).
      *����������������ݴĶ����ܡݶ��
                 06  LNK-NYULIFE2-KAN-MONEY            PIC  9(07).
      *����������������ݴĶ�������ô�ݿ��ť�����
                 06  LNK-NYULIFE2-KANFTN-SRYCD         PIC  X(09).
      *����������������ݴĶ�������ô�ݶ��
                 06  LNK-NYULIFE2-KANFTN-MONEY         PIC  9(07).
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼
      *            ����������פʤ����������������������оݳ���
           03  LNK-NYULIFE2-RC                     PIC S9(9) BINARY.
      *
