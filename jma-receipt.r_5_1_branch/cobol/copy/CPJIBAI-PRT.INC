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
      *    �����գУģ���         COPY (CPJIBAI-PRT.INC)         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    17/04/10  ����ñ�̤Ǥ�������������
      *                                     ��ϫ�ҽ�����Ϸ��ݽ����б�
      *                                     �ݸ���ʬ���ɲ�
      ************************************************************
      *01  JIBAI-PRT-REC.
      *�ˣţ�
           03  JIBAI-PRT-KEY.
               05  JIBAI-PRT-KEY1.
      *���ŵ��أɣ�
                   07  JIBAI-PRT-HOSPNUM   PIC 9(2).
      *����ǯ��
                   07  JIBAI-PRT-SRYYM     PIC 9(06).
      *������ʬ
                   07  JIBAI-PRT-NYUGAIKBN PIC X(01).
      *���ԣɣ�
                   07  JIBAI-PRT-PTID      PIC 9(10).
      *�쥻�ץȲ�
                   07  JIBAI-PRT-RECEKA    PIC X(02).
      *������ʧ����
                   07  JIBAI-PRT-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  JIBAI-PRT-RECESYUBETU
                                           PIC X(04).
      *�ݸ����ֹ�
                   07  JIBAI-PRT-HKNJANUM  PIC X(08).
      *�����ʬ
                   07  JIBAI-PRT-HOJOKBN-KEY
                                           PIC X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
                   07  JIBAI-PRT-KOHID-KEY PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  JIBAI-PRT-TEKSTYMD  PIC X(08).
      *ʬ���ʬ
                   07  JIBAI-PRT-RECEIPT-KBN
                                           PIC X(10).
      *�ǡ���Ϣ��
               05  JIBAI-PRT-RENNUM        PIC 9(03).
      *�����ֹ�
           03  JIBAI-PRT-PTNUM             PIC X(20).
      *�ݸ��ֹ�
           03  JIBAI-PRT-HKNNUM            PIC X(03).
      *�ͼ���ʬ�ʣ������ͼ������������ͼ��������軰�Թ԰١�
           03  JIBAI-PRT-RECEKBN           PIC X(01).
      *�ݸ���ʬ�ʣ��������ա�ϫ�ҽ��ˡ����������աʷ��ݽ��ˡ�
           03  JIBAI-PRT-HKNKBN            PIC X(01).
      *Ģɼ��ʬ
           03  JIBAI-PRT-PRTKBN            PIC X(03).
      *����ǯ����
           03  JIBAI-PRT-CREYMD            PIC X(08).
      *��������
           03  JIBAI-PRT-CREHMS            PIC X(06).
      *Ģɼ�ɣ�
           03  JIBAI-PRT-FORMID            PIC X(20).
      *Ģɼ�ǡ���
           03  JIBAI-PRT-PRTDATA           PIC X(10000).
