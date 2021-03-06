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
      *    �ݸ��ȹ礻�Խ��ѥ�᥿����COPY (CPORCSHKNSET.INC)
      *                            ���ѥץ������ 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  05/08/17  �ݸ����ֹ桢����ɣ��ֵ��ɲ�
      * 04.01.00     NACL-¿��Ǽ  07/11/12  ������ʬ������ɽ�����ɲ�
      * 05.00.00     NACL-¿��Ǽ  19/02/08  ������ʬ�� ����������ɲ�
      ************************************************************
       01  ORCSHKNSETAREA.
      *
      *����
           03  ORCSHKN-IN-AREA.
      *������ʬ�ʣ��������оݡ�����������
               05  ORCSHKN-KBN                 PIC  X(01).
      *������ʬ���ʣ������ޤȤ�����ͭ���
               05  ORCSHKN-KBN2                PIC  X(01).
      *������ʬ���ʣ�����ɽ��ʬͭ���
               05  ORCSHKN-KBN3                PIC  X(01).
      *������ʬ���ʣ���MAX 200�� ��
               05  ORCSHKN-KBN4                PIC  X(01).
      *
      *�ݸ��ȹ礻����
           03  ORCSHKN-OUT-AREA.
      *        ������
               05  ORCSHKN-HKN-MAX             PIC 9(04).
      *        ���ɽ������
               05  ORCSHKN-IDX                 PIC 9(04).
      *
      *        ��������ɽ��
               05  ORCSHKN-GMN-HKNCOMBI-LISTG.
                   07  ORCSHKN-GMN-HKN-LIST    OCCURS  200.
      *                �ݸ��ȹ��ֹ�
                       09  ORCSHKN-GMN-HKNCOMBINUM     PIC  X(04).
      *                �ݸ�̾�Ρ�ϫ�Ҥ����̤ϣ���ʸ���ޤǡ�
                       09  ORCSHKN-GMN-HKNCOMBIMEI     PIC  X(50).
      *                ��ô����ZZZ%��
                       09  ORCSHKN-GMN-FTNRATEMEI      PIC  X(05).
      *                Ŭ�Ѵ���
                       09  ORCSHKN-GMN-TEKSTYMD        PIC  X(09).
                       09  ORCSHKN-GMN-TEKEDYMD        PIC  X(09).
      *                ����ɽ��
      *                Ŭ�Ѵ���
                       09  ORCSHKN-NAI-TEKSTYMD        PIC  X(08).
                       09  ORCSHKN-NAI-TEKEDYMD        PIC  X(08).
      *                ��ô���
                       09  ORCSHKN-NAI-FTNRATE         PIC  9(03).
      *                �ݸ��ɣ�
                       09  ORCSHKN-NAI-HKNID           PIC  9(10).
      *                �ݸ�����
                       09  ORCSHKN-NAI-HKNNUM          PIC  X(03).
      *                �����ʬ
                       09  ORCSHKN-NAI-HOJOKBN         PIC  X(01).
      *                �ܿͲ�²��ʬ
                       09  ORCSHKN-NAI-HONKZKKBN       PIC  X(01).
      *                �ݸ���ʬ
                       09  ORCSHKN-NAI-HKNKBN          PIC  9(01).
      *                �ݸ����ֹ�
                       09  ORCSHKN-NAI-HKNJANUM        PIC  X(08).
      *                ϫ�ҡ��ͻ��ʬ
                       09  ORCSHKN-NAI-RSI-SISIKBN     PIC  X(01).
      *                ϫ�ҡ�����ǯ����
                       09  ORCSHKN-NAI-RSI-SHOBYOYMD   PIC  X(08).
      *                ϫ�ҡ��ݸ���ʬ
                       09  ORCSHKN-NAI-RSI-HKNKBN      PIC  X(01).
      *                ��������μ�����ֹ�
                       09  ORCSHKN-NAI-JKYSNUM         PIC  X(20).
      *                ������ʬ
                       09  ORCSHKN-NAI-CHIKENKBN       PIC  X(01).
      *                ������ࡦ����ɣ�
                       09  ORCSHKN-NAI-KOH-G.
                           11  ORCSHKN-NAI-KOH1HKNNUM      PIC  X(03).
                           11  ORCSHKN-NAI-KOH1ID          PIC  9(10).
                           11  ORCSHKN-NAI-KOH2HKNNUM      PIC  X(03).
                           11  ORCSHKN-NAI-KOH2ID          PIC  9(10).
                           11  ORCSHKN-NAI-KOH3HKNNUM      PIC  X(03).
                           11  ORCSHKN-NAI-KOH3ID          PIC  9(10).
                           11  ORCSHKN-NAI-KOH4HKNNUM      PIC  X(03).
                           11  ORCSHKN-NAI-KOH4ID          PIC  9(10).
                       09  ORCSHKN-NAI-KOH-R       REDEFINES
                                                   ORCSHKN-NAI-KOH-G.
                           11  ORCSHKN-NAI-KOH-OCC     OCCURS   4.
                               13  ORCSHKN-NAI-KOHHKNNUM   PIC  X(03).
                               13  ORCSHKN-NAI-KOHID       PIC  9(10).
      *                ��ɽ����ʬ
                       09  ORCSHKN-NAI-HYOJIKBN        PIC  X(01).
      *���顼������
           03  ORCSHKN-ERRCD                    PIC 9(01).
      *
