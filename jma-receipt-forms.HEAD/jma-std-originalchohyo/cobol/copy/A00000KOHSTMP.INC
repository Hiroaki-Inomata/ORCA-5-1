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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     @@@@-���    11/01/07  ������������
      * 01.00.02     @@@@-���    19/08/30  ���������ô�ԵڤӼ�����ֹ椬�Ѥ�ä���硢
      *                                     ʬ����ư��������褦�˽���
      ********************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *���ŵ��أɣ�
      *         05  MF100-HOSPID           PIC  X(24).
      *���ŵ����ֹ�
               05  MF100-HOSPNUM          PIC  9(02).
      *�����ֹ�(�����)
               05  MF100-KOHNUM-KEY       PIC  X(03).
      *�����ֹ�
               05  MF100-PTNUM            PIC  X(20).
      *����ǯ��
               05  MF100-SRYYM            PIC  9(06).
      *������ʬ
               05  MF100-NYUGAIKBN        PIC  X(01).
      *�쥻�ץȲʡʶ���λ������򥻥åȡ�
               05  MF100-SRYKA            PIC  X(02).
      *������ʧ����
               05  MF100-TEISYUTUSAKI     PIC  9(01).
      *�쥻�ץȼ���
               05  MF100-RECESYUBETU      PIC  X(04).
      *�ݸ����ֹ�
               05  MF100-HKNJANUM         PIC  X(08).
      *Ŭ�ѳ���ǯ����
               05  MF100-TEKSTYMD         PIC  X(08).
      *��ô���ֹ�
               05  MF100-FTNJANUM         PIC  X(20).
      *������ֹ�
               05  MF100-JKYSNUM          PIC  X(20).
      *���ԣɣ�
           03  MF100-PTID                PIC  9(10).
      *������̾
           03  MF100-NAME                PIC  X(100).
      *���ʻ�̾
           03  MF100-FURIGANA            PIC  X(100).
      *����
           03  MF100-SEX                 PIC  9(01).
      *���ݸ�����
           03  MF100-HKN.
      *���ݸ��ݵ���
               05  MF100-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  MF100-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  MF100-KYURATE         PIC  9(03).
      *Ϸ���ݸ��ֹ�
           03  MF100-RJNHKNNUM           PIC  X(03).
      *�����ֹ�
           03  MF100-KOHNUM              PIC  X(03).
      *����̾
           03  MF100-KOHNAME             PIC  X(20).
      *����������
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *�����������
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *��ô��۾���
           03  MF100-FTNMONEYINF.
               05  MF100-FTNMONEY        PIC  9(09).
      *����������-�������
           03  MF100-SKJKAISUINF.
               05  MF100-SKJKAISU        PIC  9(02).
      *����������-�������
           03  MF100-SKJSKYINF.
               05  MF100-SKJSKY          PIC  9(09).
      *����������-ɸ����ô����
           03  MF100-SKJFTNINF.
               05  MF100-SKJFTN          PIC  9(09).
      *¾ˡ����(�����ޤ�)
           03  MF100-TAHO-TBL.
               05  MF100-TAHO            PIC  X(03) OCCURS 4.
      *Ĺ��
           03  MF100-TYOKI               PIC  9(01).