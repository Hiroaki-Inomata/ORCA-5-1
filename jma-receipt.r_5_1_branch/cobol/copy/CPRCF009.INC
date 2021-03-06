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
      *    �쥻�ץ����ٽ�     COPY (CPRCF009.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.06.01    NACL-ƣ��    11/02/01  ���ݸ����ʤ���硢��Ƹ�ݸ�(53)
      *                                     �ϸ���ñ�ȤΥ쥻�ץȤȤ���
      *                                     ������kohnum �ɲ�
      *  04.06.02    NACL-ƣ��    12/09/27  �����ݸ���ֽ�ʣ�б�
      ************************************************************
      *01  RECE09-REC.
           03  RECE09-KEY.
      *���ŵ��أɣ�
               05  RECE09-HOSPNUM         PIC 9(2).
      *����ǯ��
               05  RECE09-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE09-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE09-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE09-RECEKA          PIC  X(02).
      *�����ֹ�
               05  RECE09-KOHNUM          PIC  X(03).
      *����ɣ�
               05  RECE09-KOHID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE09-TEKSTYMD        PIC  X(08). 
      *���ݸ�����
           03  RECE09-HKN.
      *���ݸ����ݸ��ɣ�
               05  RECE09-HKNID           PIC  9(10).
      *���ݸ����ݸ��ֹ�
               05  RECE09-HKNNUM          PIC  X(03).
      *���ݸ���ˡ���ֹ�
               05  RECE09-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  RECE09-HKNJANUM        PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  RECE09-HONKZKKBN       PIC  X(01).
      *���ݸ��������ʬ
               05  RECE09-HOJOKBN         PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  RECE09-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  RECE09-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  RECE09-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  RECE09-KYURATE         PIC  9(03).
      *���ݸ��ݾ������ֹ�
               05  RECE09-CERTNUM         PIC  X(03).
      *���ݸ���û������̾
               05 RECE09-TANSEIDONAME      PIC  X(20).
      *������ʧ����
           03  RECE09-TEISYUTUSAKI        PIC  9(01).
      *Ŭ�ѳ���ǯ����
           03  RECE09-TEKSTYMDX           PIC  X(08). 
      *Ŭ�ѽ�λǯ����    
           03  RECE09-TEKEDYMDX           PIC  X(08). 
