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
      *    �쥻�ץȴ���                       (CPRCF007.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     02.02.12  �Ϳ����ɲ�
      *  01.00.02    MCC-ƣ��     02.02.25  �����ե����뤫��ģ¤��ѹ�
      *
      *  01.01.01    NACL-ƣ��    02.09.26  ������ʬ��������ʬ���ɲ�
      *  01.01.02    NACL-ƣ��    05.01.13  �������̡����ջ�����ɲ�
      *
      *  02.07.02    NACL-ƣ��    05/12/12  ���ݥ쥻�ץ����ֽ��б�
      *                                     ���⸩��������ʬ
      *                                     ��ʥ�����
      *                                     �ݸ��԰�����ʬ
      *                                     �쥻�ץȼ��̰�����ʬ
      *                                     �ݸ����ֹ�
      *                                     �����ǡ�Ĺ����Ĺ������������
      *                                     �����ǡ�Ĺ���Υڡ���
      *
      *  03.04.01    NACL-ƣ��    07/03/01  ���ݥ쥻�ץ����ֽ�ܺ������б�
      *                                     �����ǰ��������ʬ
      *                                     Ĺ����Ĺ�����������˰��������ʬ
      *                                     ��������ե饰
      *                                     ���������
      *
      *  04.03.01    NACL-ƣ��    08/07/14  ���쥻�����ʱ��������ޤ���б�
      ***********************************************************
      *01  RECE07-REC.
      *�ˣţ�
           03  RECE07-KEY.
      *���ŵ��أɣ�
               05  RECE07-HOSPNUM          PIC 9(2).
      *Ģɼ�ɣ�
               05  RECE07-PRTID            PIC  X(04).
      *������ʬ�ʣ�����硢�������̡�
               05  RECE07-SYOKBN           PIC  X(01).
      *������
               05  RECE07-CREYMD           PIC  X(08).
      *��������
               05  RECE07-CREHMS           PIC  X(06).
      *������ʧ����
               05  RECE07-TEISYUTUSAKI     PIC  X(01).
      *��ʥ����ɡʼ��ݤΤ߻��ѡ�
               05  RECE07-SRYKA            PIC  X(02).
      *���⸩��������ʬ�ʹ��ݤΤ߻��ѡ�
               05  RECE07-PREFKBN-SRT      PIC  X(02).
      *�ݸ��԰�����ʬ�ʹ��ݤΤ߻��ѡ�
               05  RECE07-HKNJANUM-SRT     PIC  X(01).
      *�ݸ����ֹ�ʹ��ݤΤ߻��ѡ�99999999�����ֽ����̵���Ǻ�����        
               05  RECE07-HKNJANUM         PIC  X(08).
      *�쥻�ץȼ���
               05  RECE07-RECESYUBETU      PIC  X(04).
      *�쥻�ץȼ��̰�����ʬ�ʹ��ݤΤ߻��ѡ�
               05  RECE07-RECESYUBETU-SRT  PIC  X(03).
      *�����ǡ�̤���ѡ�
               05  RECE07-ZAITAKU          PIC  X(01).
      *Ĺ����Ĺ�����������ˡ�̤���ѡ�
               05  RECE07-CHOUKI           PIC  X(01).
      *�������
           03  RECE07-KOUGAKU              PIC  X(01).
      *������ʬ
           03  RECE07-NYUGAIKBN            PIC  X(01).
      *����ǯ��ʰ�硧����ǯ����̡���Ƭ�ο���ǯ���
           03  RECE07-SRYYM                PIC  X(06).
      *�ڡ���
           03  RECE07-PAGE                 PIC  9(05).  
      *�ڡ���
           03  RECE07-PAGE-ZAITAKU         PIC  9(05).  
      *�ڡ���
           03  RECE07-PAGE-CHOUKI          PIC  9(05).  
      *�ڡ���
           03  RECE07-PAGE-KOUGAKU         PIC  9(05).  
      *�ڡ���
           03  RECE07-PAGE-DUP             PIC  9(05).  
      *���
           03  RECE07-KENSU                PIC  9(05).
      *�ǡ�����ʬ�ʣ�:���� ��:���� ��:���ݡ�
           03  RECE07-RECEKBN              PIC  X(01).
      *��������
           03  RECE07-SYORISBT             PIC  X(01).
      *���ջ���ʳ��ϡ�
           03  RECE07-STYMD                PIC  X(08).
      *���ջ���ʽ�λ��
           03  RECE07-EDYMD                PIC  X(08).
      *���ֽ����
           03  RECE07-HSEQNUM              PIC  9(08).
      *��Ƭ���̡ʾܺ��������
           03  RECE07-SYUBETU-SRT1         PIC  X(04).
      *���ֽ�ܺ�����
           03  RECE07-RECE-PUTH.
      *���⸩����ʬ
               05  RECE07-PREFKBN          PIC  X(01).
      *������
               05  RECE07-ZAITAKU-KBN      PIC  X(01).
      *Ĺ����Ĺ������������
               05  RECE07-CHOUKI-KBN       PIC  X(01).
      *�ǽ����Ͻ�
               05  RECE07-SRT-LAST         PIC  X(01).
      *�������
               05  RECE07-KOUGAKU-TEN      PIC  9(07).
      *�쥻�ץȺ�����ʬ�ʣ��������ѡ�
               05  RECE07-PRTKBN           PIC  X(01).
      *ü���ɣ�
           03  RECE07-TERMID               PIC  X(64).
      *���ڥ졼���ɣ�
           03  RECE07-OPID                 PIC  X(16).