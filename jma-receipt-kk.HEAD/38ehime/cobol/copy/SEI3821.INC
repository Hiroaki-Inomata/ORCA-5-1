      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *          ������������ʣ�̼��쥻�� COPY (CPRMF10.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *    �ˣţ�
      *    ˡ���ֹ��ѥ�����KEY(81,82,83�ν�)
               05  MF100-SORT1               PIC  9(01).
      *    ����Ѷ�ʬ(������ץǡ��� �������̥ǡ���)
               05  MF100-KBN                 PIC  9(01).
      *    ��Į¼�ֹ�
               05  MF100-CITYNUM             PIC  X(05).
      *    ��ô���ֹ�߽���KEY(8*�����7*����)
               05  MF100-SORT2               PIC  9(01).
      *    ��ô���ֹ�
           03  MF100-FTNJANUM                PIC  X(08).
      *    ���ᥨ�ꥢ
           03  MF100-SEIKYU-AREA.
               05  MF100-HKN-TBL  OCCURS 2.
      *    ���
                   07  MF100-KENSU           PIC  9(05).
      *    ����
                   07  MF100-SEIKYUGK        PIC  9(09).
      *    ��׻�Į¼����������ѥ��ꥢ
           03  MF100-CNT-AREA.
               05  MF100-CNT-CITY            PIC  9(03).
               05  MF100-CNT-TBL  OCCURS 100.
                   07  MF100-CNT-CITYNUM     PIC  X(05).
