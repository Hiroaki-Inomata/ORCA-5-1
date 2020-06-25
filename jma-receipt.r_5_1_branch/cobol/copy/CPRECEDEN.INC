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
      *    �쥻�ץ��Ż��ޥ�����   COPY (CPRECEDEN.INC)            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  ver.2.9.1
      *  02.09.01    NACL-ƣ��    06/05/08  �����̿��壱����������
      *                                     �ɾ��ܵ��쥳�����б�
      *                                     �쥻�ťǡ����򣲣��������ѹ�
      *
      *  03.00.01    NACL-ƣ��    06/06/15  �ɾ��ܵ���ʬ���ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *                                     Ĵ���Ѥ�crehms,updhms �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *                                     receipt_kbn �ɲ� 
      ************************************************************
       01  RECEDEN-REC.
      *�ˣţ�
           03  RECEDEN-KEY.
      *���ŵ��أɣ�
               05  RECEDEN-HOSPNUM           PIC 9(2).
      *����ǯ��
               05  RECEDEN-SRYYM            PIC  9(06).
      *������ʬ
               05  RECEDEN-NYUGAIKBN        PIC  X(01).
      *���ԣɣ�
               05  RECEDEN-PTID             PIC  9(10).
      *�쥻�ץȲʡʶ���λ������򥻥åȡ�
               05  RECEDEN-RECEKA           PIC  X(02).
      *������ʧ����
               05  RECEDEN-TEISYUTUSAKI     PIC  9(01).
      *�쥻�ץȼ���
               05  RECEDEN-RECESYUBETU      PIC  X(04).
      *�ݸ����ֹ�
               05  RECEDEN-HKNJANUM         PIC  X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  RECEDEN-HOJOKBN-KEY      PIC  X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
               05  RECEDEN-KOHID-KEY        PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECEDEN-TEKSTYMD         PIC  X(08).
      *ʬ���ʬ
               05  RECEDEN-RECEIPT-KBN      PIC  X(10).
      *�쥳���ɶ�ʬ�ʣ����쥻�š������ɾ��ܵ���
               05  RECEDEN-RECKBN           PIC  X(01).
      *�ǡ���Ϣ��
               05  RECEDEN-RENNUM           PIC  9(04).
      *����ǯ����
           03  RECEDEN-CREYMD               PIC  X(08).
      *��������
           03  RECEDEN-CREHMS               PIC  X(06).
      *�������
           03  RECEDEN-TOTALTEN             PIC  9(10).
      *�ɾ��ܵ���ʬ�ʥ쥳���ɶ�ʬ���Τߡ�
           03  RECEDEN-SJKBN                PIC  X(02).
      *�쥻�ťǡ���
           03  RECEDEN-RECEDATA             PIC  X(2500).