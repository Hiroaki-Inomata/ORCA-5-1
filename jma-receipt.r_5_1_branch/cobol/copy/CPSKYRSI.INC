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
      *    �������(ϫ�Ҿ���)   COPY (CPSKYRSI.INC)              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.07.01    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *  04.07.02    NACL-ƣ��    13/09/20  �軰�Թ԰��б�
      *
      *  04.08.01    NACL-ƣ��    17/04/10  ����ñ�̤Ǥ�������������
      *                                     ��ϫ�ҽ�����Ϸ��ݽ����б�
      *                                     �ݸ���ʬ�����ѹ��ܤ��ɲ�
      ************************************************************
       01  SKYRSI-REC.
      *�ˣţ�
           03  SKYRSI-KEY.
      *���ŵ����ֹ�
               05  SKYRSI-HOSPNUM          PIC 9(2).
      *����ǯ��
               05  SKYRSI-SRYYM            PIC 9(06).
      *������ʬ
               05  SKYRSI-NYUGAIKBN        PIC X(01).
      *���ԣɣ�
               05  SKYRSI-PTID             PIC 9(10).
      *�쥻�ץȲʡʶ���λ������򥻥åȡ�
               05  SKYRSI-RECEKA           PIC X(02).
      *������ʧ����
               05  SKYRSI-TEISYUTUSAKI     PIC 9(01).
      *�쥻�ץȼ���
               05  SKYRSI-RECESYUBETU      PIC X(04).
      *�ݸ����ֹ�
               05  SKYRSI-HKNJANUM         PIC X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  SKYRSI-HOJOKBN-KEY      PIC X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
               05  SKYRSI-KOHID-KEY        PIC 9(10).
      *Ŭ�ѳ���ǯ����
               05  SKYRSI-TEKSTYMD         PIC X(08).
      *ʬ���ʬ
               05  SKYRSI-RECEIPT-KBN      PIC X(10).
      *ϫ�Ҿ���
           03  SKYRSI-RSIINF.
      *�ݸ���ʬ�ʣ���û�����������¡��������ե���������������̳�ҳ���
               05  SKYRSI-HKNKBN           PIC X(01).
      *�ͼ���ʬ�ʣ������ͼ������������ͼ���
               05  SKYRSI-RECEKBN          PIC X(01).
      *ϫ�ҽ������ʬ�ե饰�ʣ����������ʬ��
               05  SKYRSI-SYOKAIFLG        PIC X(01).
      *ϫƯ�����Ľ𥳡���
               05  SKYRSI-SYOCD            PIC X(05).
      *���Ը����ֹ�
               05  SKYRSI-KOFUNUM          PIC X(14).
      *�ҳ���ʬ�ʣ�����̳�������̶С�
               05  SKYRSI-SAIGAIKBN        PIC X(01).
      *ϫ�ҥ�����
               05  SKYRSI-COMMENT          PIC X(200).
      *������ʧ���ظ����ѹ���
           03  SKYRSI-XXTEISYUTUSAKI       PIC 9(01).
      *�ݸ��ֹ渡���ѹ���
           03  SKYRSI-XXHKNNUM             PIC X(03).
      *�ݸ���ʬ�����ѹ���
           03  SKYRSI-XXHKNKBN-RSI         PIC X(01).