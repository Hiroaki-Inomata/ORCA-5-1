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
      *    �쥻�ץ����ٽ�     ���ౡ������� (CPRCF068.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    03.05.09  ��������������ʬ���ɲ�
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.04.01    NACL-ƣ��    09/12/10  Ʊ��˽��ˤ��������ʣ��
      *                                     ������ε�Ͽ�ν��� 
      *                                     ����ֹ���ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  04.07.01    NACL-ƣ��    15/01/24  Ʊ���������������ౡ����ε�Ͽ
      *                                     ����
      *                                    �����������ౡ����Ʊ���Ȥ���
      ************************************************************
      *01  RECE68-REC.
           03  RECE68-KEY.
               05  RECE68-KEY1.
      *���ŵ��أɣ�
                   07  RECE68-HOSPNUM      PIC 9(2).
      *����ǯ��
                   07  RECE68-SRYYM        PIC 9(06).
      *������ʬ
                   07  RECE68-NYUGAIKBN    PIC X(01).
      *���ԣɣ�
                   07  RECE68-PTID         PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE68-RECEKA       PIC X(02).
      *������ʧ����
                   07  RECE68-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE68-RECESYUBETU  PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE68-HKNJANUM     PIC X(08).
      *�����ʬ
                   07  RECE68-HOJOKBN      PIC X(01).
      *����ɣ�
                   07  RECE68-KOHID        PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE68-TEKSTYMD     PIC X(08). 
      *ʬ���ʬ
                   07  RECE68-RECEIPT-KBN  PIC X(10).
      *����ǯ����
               05  RECE68-NYUINYMD        PIC X(08).
      *����ǯ����Ϣ��
               05  RECE68-NYUINYMD-SEQNO  PIC 9(01).
      *�ౡǯ����
           03  RECE68-TAIINYMD            PIC X(08).
      *��������������ʬ
           03  RECE68-NYUINCHUKBN         PIC  X(01).
      *����ֹ������ǯ������Ʊ���Ȥ��������åȡ�
           03  RECE68-SHONUM              PIC  9(03).