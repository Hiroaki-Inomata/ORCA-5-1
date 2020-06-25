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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SKYSRH-REC.
      *    ���ŵ����ֹ�
           03  SKYSRH-HOSPNUM          PIC 9(02).
      *    ü���ɣ�
           03  SKYSRH-TERMID           PIC X(64).
      *    ���ԣɣ�
           03  SKYSRH-PTID             PIC 9(10).
      *    ������ʬ
           03  SKYSRH-NYUGAIKBN        PIC X(1).
      *    ����ǯ��
           03  SKYSRH-SRYYM            PIC X(6).
      *    �쥻�ץȲʡʶ���λ������򥻥åȡ�
           03  SKYSRH-RECEKA           PIC  X(02).
      *    ������ʧ����
           03  SKYSRH-TEISYUTUSAKI     PIC  9(01).
      *    �쥻�ץȼ���
           03  SKYSRH-RECESYUBETU      PIC  X(04).
      *    �ݸ����ֹ�
           03  SKYSRH-HKNJANUM         PIC  X(08).
      *    �����ʬ�������ݸ��ΤȤ��Τߡ�
           03  SKYSRH-HOJOKBN-KEY      PIC  X(01).
      *    ����ɣġʰ��ݡ�����ñ�ȤΤߡ�ϫ�ҡ������ա��ݸ��ɣġ�
           03  SKYSRH-KOHID-KEY        PIC  9(10).
      *    Ŭ�ѳ���ǯ����
           03  SKYSRH-TEKSTYMD         PIC  X(08).
      *    ʬ���ʬ
           03  SKYSRH-RECEIPT-KBN      PIC  X(10).
      *    �ݸ��ɣ�
           03  SKYSRH-HKNID            PIC 9(10).
      *    �ݸ��ֹ�
           03  SKYSRH-HKNNUM           PIC X(3).
      *    �������
           03  SKYSRH-KOH-G.
               05  SKYSRH-KOH-OCC      OCCURS 4.
      *    ����ɣ�
                   07  SKYSRH-KOHID    PIC 9(10).
      *    �����ֹ�
                   07  SKYSRH-KOHNUM   PIC X(3).
      *    �ݸ�����
           03  SKYSRH-TOTALTEN         PIC 9(10).
      *    �õ������ֹ�
           03  SKYSRH-TOKKI-G.
               05  SKYSRH-TOKKI        PIC X(2)  OCCURS 5.
      *    �õ�����̾��
           03  SKYSRH-TOKKINM-G.
               05  SKYSRH-TOKKINM      PIC X(12) OCCURS 5.
      *    ���ܳ���������ް�����ô�����
           03  SKYSRH-ETC-YKZFTN1      PIC  9(05).
      *    �����ʬ�ʥ쥻���оݡ�
           03  SKYSRH-SKYKBN-RECEDEN   PIC X(1).
      *    �쥻�ץȵ��ܳ������������ե饰��1:���ܳ�������������
           03  SKYSRH-ETCCREATEFLG     PIC  9(01).
      *    �ǡ�����ʬ(1:�������/2:��Ǽ��
           03  SKYSRH-DATAKBN          PIC X(1).
      *    Ϣ��(������������������ǡ������Ф��ƺ��֡�
           03  SKYSRH-SEQNUM           PIC 9(7).
      *    ������
           03  SKYSRH-CREYMD           PIC X(8).
      *    ������
           03  SKYSRH-UPYMD            PIC X(8).
      *    ��������
           03  SKYSRH-UPHMS            PIC X(6).
      *    ���Ͽ���ǯ��
           03  SKYSRH-STSRYYM          PIC X(6).
      *    ��λ����ǯ��
           03  SKYSRH-EDSRYYM          PIC X(6).
      *    ���աܻ���
           03  SKYSRH-YMDHMS           PIC X(14).
      *    �ݸ��������
           03  SKYSRH-TOTALTEN-MAX     PIC 9(10).