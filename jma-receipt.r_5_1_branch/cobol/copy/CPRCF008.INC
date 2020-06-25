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
      *    �쥻�ץ����ٽ�     COPY (CPRCF008.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.06.29  Ŭ�ѳ���ǯ�������ɲ�
      *  01.01.01    MCC-ƣ��     02.01.29  �ե�����쥤�������ѹ�
      *  01.01.02    MCC-ƣ��     02.05.16  ���̤λ�ʣ�����ŷ�Ǻ������뤿���
      *                                     ����ǯ����ɲ�
      *  01.01.03    MCC-��Ʀ��   02.05.27  ���ݤν��Ͻ��ѹ��Ѥ��ݸ���ʬ�ɲ�
      *                                     �������ܡ�����������������������
      *                                     �����������ۡ��������ѡ������ȹ�
      *                                     �������Ҵ��������������ȹ�
      *  01.01.04    MCC-ƣ��     02.07.01  Ģɼ�ǡ���X(10000)���ѹ�
      *                                     RECENUM��ˣţٹ��ܤ��ѹ�      
      *
      *  01.02.01    NACL-ƣ��    02.09.26  Ģɼ�ɣġ�������ʬ���ɲ�
      *
      *  02.06.01    NACL-ƣ��    05.07.13  �����ѹ��ܤ�Ϣ�֤��ɲ�
      *
      *  02.07.02    NACL-ƣ��    05/12/12  ���ݥ쥻�ץ����ֽ��б�
      *                                     �����ǡ��߰���
      *                                     Ĺ����Ĺ������������  
      *                                     ���⸩��������ʬ
      *                                     �ݸ��԰�����ʬ
      *                                     �쥻�ץȼ��̰�����ʬ
      *                                     ��ʥ�����
      *                                     �ݸ����ֹ�ʸ����ѡ�
      *
      *  03.00.01    NACL-ƣ��    06/05/29  �������Υץ�󥿡������б�
      *                                     �ץ��̾�ɲ�
      *                                     Ϣ�֤�Ĺ���ѹ�
      *
      *  03.02.01    NACL-ƣ��    06/08/31  ����ѽ��Ͻ缱���ֹ��ɲ�
      *
      *  03.02.01    NACL-ƣ��    07/03/01  �������־ܺ������б�
      *                                     ���̽��Ͻ�
      *                                     ���롼�����꣱
      *                                     �ܿͲ�²��ʬ
      *                                     ������Ͻ� 
      *                                     ��������Ͻ磱
      *                                     ���롼�����ꣲ
      *                                     ��������Ͻ磲
      *                                     ���Ͻ� 
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.03.01    NACL-ƣ��    08/06/19  ����̽����б�
      *                                     ��ե����ɡ�����̿��Ų�
      *
      *  04.05.01    NACL-ƣ��    09/06/11  �����̽����б�
      *
      *  04.07.01    NACL-ƣ��    12/12/03  �����ߥʥ�ǥ��åȤˤ������б�
      *************************************************************
       01  RECE08-REC.
      *�ˣţ�
           03  RECE08-KEY.
      *���ŵ��أɣ�
               05  RECE08-HOSPNUM          PIC 9(2).
      *Ģɼ�ɣġʸ����ѣˣţ١�
               05  RECE08-PRTID            PIC  X(04).
      *����ǯ��ʸ����ѣˣţ١���硧����ǯ����̡�������
               05  RECE08-RECEYM           PIC  X(06).
      *������
               05  RECE08-CREYMD           PIC  X(08).
      *��������
               05  RECE08-CREHMS           PIC  X(06).
      *������ʧ����
               05  RECE08-TEISYUTUSAKI     PIC  X(01).
      *��ʥ�����
               05  RECE08-SRYKA            PIC  X(02).
      *���⸩��������ʬ�ʹ��ݤΤ߻��ѡ�
               05  RECE08-PREFKBN-SRT      PIC  X(02).
      *�ݸ��԰�����ʬ�ʹ��ݤΤ߻��ѡ�
               05  RECE08-HKNJANUM-SRT     PIC  X(01).
      *�ݸ����ֹ�        
               05  RECE08-HKNJANUM         PIC  X(08).
      *Ŭ�ѳ���ǯ����
               05  RECE08-TEKSTYMD         PIC  X(08). 
      *�쥻�ץȼ��̰�����ʬ
               05  RECE08-RECESYUBETU-SRT  PIC  X(03).
      *�쥻�ץȼ���
               05  RECE08-RECESYUBETU      PIC  X(04).
      *���⡦������ʬ�ʣ������⡡������������
               05  RECE08-PREFKBN          PIC  X(01).
      *���ֹ�
               05  RECE08-PREFNUM          PIC  X(02).
      *�����ֹ�
               05  RECE08-PTNUM            PIC  X(20).
      *����ǯ��
               05  RECE08-SRYYM            PIC  X(06).
      *Ϣ��
               05  RECE08-RENNUM           PIC  9(03).
      *Ϣ�ֹ�
               05  RECE08-RECENUM          PIC  9(06).
      *�ݸ���ʬ�ʼ�����л��Τߡ�
           03  RECE08-HKNKBN               PIC  X(01).
      *���ԥ��ʻ�̾
           03  RECE08-KANANAME             PIC  X(100).
      *���Ͻ缱���ֹ�
           03  RECE08-PRTJYUN              PIC  9(02).
      *���Ͻ�Ϣ��
           03  RECE08-PRTRENNUM            PIC  9(06).
      *������ʬ
           03  RECE08-NYUGAIKBN            PIC  X(01).
      *�������
           03  RECE08-KOUGAKU              PIC  X(01).
      *�����ǡ��߰���
           03  RECE08-ZAITAKU              PIC  X(01).
      *Ĺ����Ĺ������������  
           03  RECE08-CHOUKI               PIC  X(01).
      *�������ݸ����ֹ�        
           03  RECE08-HKNJANUM-S           PIC  X(08).
      *
           03  RECE08-SYUBETU-SRT1         PIC S9(9)   BINARY.
      *
           03  RECE08-GRP-NO1              PIC 9(3).
      *
           03  RECE08-HONKZKKBN            PIC X(1).
      *
           03  RECE08-KOUHI-NO             PIC S9(3).
      *
           03  RECE08-SYUBETU-UNDER-1.
               05  RECE08-SYUBETU-UNDER-1-1
                                           PIC X(40).
               05  RECE08-SYUBETU-UNDER-1-2
                                           PIC X(40).
      *
           03  RECE08-GRP-NO2              PIC X(3).
      *
           03  RECE08-SYUBETU-UNDER-2      PIC X(40).
      *
           03  RECE08-SYUBETU-SRT2         PIC S9(9)   BINARY.
      *����̾���
           03  RECE08-DRCD-AREA.
      *��ե�����
               05  RECE08-DRCD             PIC  X(05).
      *����̿��Ų�
               05  RECE08-DRCD-SRYKA       PIC  X(02).
      *�����
           03  RECE08-SORT-1               PIC  X(10).
      *�������
           03  RECE08-SORT-2               PIC  X(50).
      *Ģɼ�ɣ�
           03  RECE08-FORMID               PIC  X(20).
      *Ģɼ�ǡ���
           03  RECE08-PRTDATA              PIC  X(10000).
      *�ץ��̾
           03  RECE08-PRTNM                PIC  X(16).
      *ü���ɣ�
           03  RECE08-TERMID               PIC  X(64).
      *���ڥ졼���ɣ�
           03  RECE08-OPID                 PIC  X(16).
      *����ѽ��Ͻ缱���ֹ�
           03  RECE08-PRTJYUN-SYUKA        PIC  9(4).
      *�����ѹ���
           03  RECE08-STKANANAME           PIC  X(100).
           03  RECE08-EDKANANAME           PIC  X(100).
           03  RECE08-STRENNUM             PIC  9(02).
           03  RECE08-EDRENNUM             PIC  9(02).