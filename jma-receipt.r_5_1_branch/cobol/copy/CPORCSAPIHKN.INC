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
      *    ���У��ݸ��ȹ礻�����ѥ�᥿����COPY (CPORCSHKNGET.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.07.00     NACL-¿��Ǽ  14/11/12  ��ǧ����¾�ɲ�
      * 04.08.00     NACL-¿��Ǽ  17/01/XX  ������ʬ�����ݸ��ȹ礻�����ɲ�
      * 05.00.00     NACL-¿��Ǽ  18/11/XX  ���������ɲ�
      * 05.00.00     NACL-¿��Ǽ  19/06/XX  �����ǧ�����ɲ�
      * 05.00.00     NACL-¿��Ǽ  20/04/XX  ���ݸ��Ծڻ����ɲ�
      ************************************************************
       01  ORCSAPIHKNAREA.
      *����
      *������ʬ�ʣ�����ư���򤹤��
           03  ORCSAPIHKN-KBN              PIC  X(01).
      *���Ųʡ���������������
           03  ORCSAPIHKN-SRYKA            PIC  X(02).
      *������ʬ���ʣ���������Ͽ���ʤ��٤ơˡ�����ǯ���ϰϡ��ʳ���������
           03  ORCSAPIHKN-KBN2             PIC  X(01).
      *H29.1
      *������ʬ���ʣ����оݣ�������ޤǡ�
           03  ORCSAPIHKN-KBN3             PIC  X(01).
      *H30.11
      *�������ա�̤����ϥ����ƥ����ա�
           03  ORCSAPIHKN-SRYYMD           PIC  X(08).
      *�ݸ�����
           03  ORCSAPIHKN-IN-HKNAREA.
      *        �ݸ�����
               05  ORCSAPIHKN-IN-HKNNUM            PIC X(03).
      *        �ݸ����ֹ�
               05  ORCSAPIHKN-IN-HKNJANUM          PIC X(08).
      *        �ݸ�����̾��
               05  ORCSAPIHKN-IN-HKNNUM-NAME       PIC X(10).
      *        ����
               05  ORCSAPIHKN-IN-MARK              PIC X(80).
      *        �ֹ�
               05  ORCSAPIHKN-IN-NUMBER            PIC X(80).
      *        ͭ������
               05  ORCSAPIHKN-IN-TEKSTYMD          PIC X(08).
               05  ORCSAPIHKN-IN-TEKEDYMD          PIC X(08).
      *        �������
               05  ORCSAPIHKN-IN-INSURANCE-INFO
                                       OCCURS  4 TIMES.
                   07  ORCSAPIHKN-IN-KOHNUM            PIC X(04).
                   07  ORCSAPIHKN-IN-KOHNUM-NAME       PIC X(10).
                   07  ORCSAPIHKN-IN-FTNJANUM          PIC X(08).
                   07  ORCSAPIHKN-IN-JKYSNUM           PIC X(20).
      *    �ݸ��ȹ礻
           03  ORCSAPIHKN-IN-HKNCOMBI      PIC X(04).
      *
      *�����ΰ�
           03  ORCSAPIHKN-OUT-AREA.
      *        �о��ݸ��ȹ礻
               05  ORCSAPIHKN-OT-HKNCOMBI      PIC X(04).
      *ͭ���ݸ�����
           03  ORCSAPIHKN-HKNCOMBI-AREA.
               05  ORCSAPIHKN-HKNCOMBI-TBL     OCCURS   200.
      *            �ݸ��ȹ礻
                   07  ORCSAPIHKN-THKNCOMBI        PIC  X(04).
      *H29.1
      *            �ݸ��ȹ礻��ô���ʳ����
                   07  ORCSAPIHKN-COMB-RATEG       PIC  9(03).
      *            �ݸ��ȹ礻��ô����������
                   07  ORCSAPIHKN-COMB-RATEN       PIC  9(03).
      *            �ݸ��ȹ礻ͭ������
                   07  ORCSAPIHKN-COMB-TEKSTYMD    PIC  X(10).
                   07  ORCSAPIHKN-COMB-TEKEDYMD    PIC  X(10).
                   07  ORCSAPIHKN-COMBN-TEKSTYMD   PIC  X(08).
                   07  ORCSAPIHKN-COMBN-TEKEDYMD   PIC  X(08).
      *
      *            �ݸ�����
                   07  ORCSAPIHKN-HKNNUM           PIC  X(03).
      *            �ݸ�����̾��
                   07  ORCSAPIHKN-HKNNUM-NAME      PIC  X(20).
      *            �ݸ����ֹ�
                   07  ORCSAPIHKN-HKNJANUM         PIC  X(08).
      *            ����
                   07  ORCSAPIHKN-KIGO             PIC  X(80).
      *            �ֹ�
                   07  ORCSAPIHKN-NUM              PIC  X(80).
      *R02.4       �ֹ����
                   07  ORCSAPIHKN-EDABAN           PIC  X(02).
      *            ��³��ʬ
                   07  ORCSAPIHKN-CONTKBN          PIC  X(01).
      *            �����ʬ
                   07  ORCSAPIHKN-HOJOKBN          PIC  X(01).
      *            �����ʬ̾��
                   07  ORCSAPIHKN-HOJOKBN-NAME     PIC  X(30).
      *            �ܿͲ�²��ʬ
                   07  ORCSAPIHKN-HONKZKKBN        PIC  X(01).
      *            ���ݸ���̾��
                   07  ORCSAPIHKN-HIHKNJANAME      PIC  X(100).
      *            Ŭ�Ѵ��֡�ɽ����
                   07  ORCSAPIHKN-TEKSTYMD         PIC  X(10).
                   07  ORCSAPIHKN-TEKEDYMD         PIC  X(10).
      *            Ŭ�Ѵ���
                   07  ORCSAPIHKN-NAI-TEKSTYMD     PIC  X(08).
                   07  ORCSAPIHKN-NAI-TEKEDYMD     PIC  X(08).
      *            ��ʼ�����
                   07  ORCSAPIHKN-SKKGETYMD        PIC  X(10).
                   07  ORCSAPIHKN-NAI-SKKGETYMD    PIC  X(08).
      *            ��ǧ����
                   07  ORCSAPIHKN-KAKUNINYMD       PIC  X(10).
                   07  ORCSAPIHKN-NAI-KAKUNINYMD   PIC  X(08).
      *            �ݸ��ɣ�
                   07  ORCSAPIHKN-NAI-HKNID        PIC  9(10).
      *            ϫ�Ҷ�ʬ
                   07  ORCSAPIHKN-RSI-HKNKBN       PIC  X(01).
      *            ��ɽ����ʬ
                   07  ORCSAPIHKN-HYOJIKBN         PIC  X(01).
      *            ������ࡦ����ɣ�
                   07  ORCSAPIHKN-KOH-G.
                       09  ORCSAPIHKN-KOH-OCC      OCCURS   4.
                           11  ORCSAPIHKN-KOHNUM           PIC  X(03).
                           11  ORCSAPIHKN-KOHNUM-NAME      PIC  X(20).
                           11  ORCSAPIHKN-FTNJANUM         PIC  X(08).
                           11  ORCSAPIHKN-JKYSNUM          PIC  X(20).
      *                    Ŭ�Ѵ���
                           11  ORCSAPIHKN-KOH-TEKSTYMD     PIC  X(10).
                           11  ORCSAPIHKN-KOH-TEKEDYMD     PIC  X(10).
      *R01.6
      *                    ��ǧ����
                           11  ORCSAPIHKN-KOH-KAKUNINYMD   PIC  X(10).
      *                    ��������ôΨ
                           11  ORCSAPIHKN-RATE-ADMISSION   PIC X(05).
      *                    �����ݸ����
                           11  ORCSAPIHKN-MONEY-ADMISSION  PIC X(06).
      *                    �������ôΨ
                           11  ORCSAPIHKN-RATE-OUTPATIENT  PIC X(05).
      *                     ����ݸ����
                           11  ORCSAPIHKN-MONEY-OUTPATIENT PIC X(06).
      *                    ����ɣ�
                           11  ORCSAPIHKN-NAI-KOHID        PIC 9(10).
      *        �о��ݸ��ȹ礻���
               05  ORCSAPIHKN-OT-MAX           PIC 9(04).
      *���顼������
           03  ORCSAPIHKN-ERRCD                    PIC 9(01).
      *
      *