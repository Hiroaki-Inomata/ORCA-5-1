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
      *****************************************************************
      *  ������̾�ޥ�����       (CPPTBYOMEI.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01/06/06  �ݸ���̾�ե饰���ɲ�
      *  01/00/02    MCC-ƣ��     01.12.06  �������쥤�����Ȥ��ѹ�     
      *
      *  ��������̾�ޥ����б�
      *  01.02.01    NACL-ƣ��    04/01/07  �������쥤�����Ȥ��ѹ�
      *ver.2.6.0 
      *  01.02.02    NACL-ƣ��    05/05/26  ��̾�����ѹ��ܤ��ɲ�
      *
      *  03.04.01    NACL-ƣ��    06/10/31  ����ե饰��������ڥ졼��
      *                                     �ɣĤ��ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/06/20  ��̾�����Ͻ��б�
      *                                     ���Ͻ�Ϣ�֤��ɲ� 
      *  04.06.02    NACL-ƣ��    10/11/05  ��­�������б�
      *
      *  04.07.01    NACL-ƣ��    13/07/12  �軰�Թ԰��б�
      *  04.07.02    NACL-ƣ��    15/07/29  ������̾��Ͽ������Ͽ���ֵ�Ͽ
      *                                     �Ȳ���ɽ���б�
      *                                     ��Ͽ���֤��ɲ�
      *
      *  04.08.01    NACL-ƣ��    16/02/17  �ౡ������ν���̾�󵭺��б�
      *                                     ��ʻ�ɤ˷��������ɲ��б�
      *****************************************************************
      *
      *01  PTBYOMEI-REC.
           03  PTBYO-KEY.
      *���ŵ��أɣ�
               05  PTBYO-HOSPNUM        PIC 9(2).
      *���ԣɣ�
               05  PTBYO-PTID          PIC 9(10).
      *���Ų�
               05  PTBYO-SRYKA         PIC X(02).
      *���ų�����
               05  PTBYO-SRYYMD        PIC X(08).
      *Ϣ�ֹ�
               05  PTBYO-RENNUM        PIC 9(02).
      *������̾������
           03  PTBYO-KHNBYOMEICD       PIC X(07).
      *��̾���ϥ�����
           03  PTBYO-BYOMEIINPUTCD-G.
               05  PTBYO-BYOMEIINPUTCD PIC X(20)
                                       OCCURS  6.
      *��̾�����ɿ�
           03  PTBYO-BYOMEICDSU        PIC 9(02).
      *��̾������
           03  PTBYO-BYOMEICD-G.
               05  PTBYO-BYOMEICD-1    PIC X(07).
               05  PTBYO-BYOMEICD-2    PIC X(07).
               05  PTBYO-BYOMEICD-3    PIC X(07).
               05  PTBYO-BYOMEICD-4    PIC X(07).
               05  PTBYO-BYOMEICD-5    PIC X(07).
               05  PTBYO-BYOMEICD-6    PIC X(07).
               05  PTBYO-BYOMEICD-7    PIC X(07).
               05  PTBYO-BYOMEICD-8    PIC X(07).
               05  PTBYO-BYOMEICD-9    PIC X(07).
               05  PTBYO-BYOMEICD-10   PIC X(07).
               05  PTBYO-BYOMEICD-11   PIC X(07).
               05  PTBYO-BYOMEICD-12   PIC X(07).
               05  PTBYO-BYOMEICD-13   PIC X(07).
               05  PTBYO-BYOMEICD-14   PIC X(07).
               05  PTBYO-BYOMEICD-15   PIC X(07).
               05  PTBYO-BYOMEICD-16   PIC X(07).
               05  PTBYO-BYOMEICD-17   PIC X(07).
               05  PTBYO-BYOMEICD-18   PIC X(07).
               05  PTBYO-BYOMEICD-19   PIC X(07).
               05  PTBYO-BYOMEICD-20   PIC X(07).
               05  PTBYO-BYOMEICD-21   PIC X(07).
      *
           03  PTBYO-BYOMEICD-R        REDEFINES   PTBYO-BYOMEICD-G.
               05  PTBYO-BYOMEICD      PIC X(07)
                                       OCCURS   21.
      *�����ե饰
           03  PTBYO-UTAGAIFLG         PIC X(01).
      *����̾�ե饰
           03  PTBYO-SYUBYOFLG         PIC X(01).
      *�����ե饰
           03  PTBYO-MANSEIKBN         PIC 9(02).
      *������ʬ
           03  PTBYO-NYUGAIKBN         PIC X(01).
      *�ݸ��ȹ礻�ֹ�
           03  PTBYO-HKNCOMBI          PIC 9(04).
      *����ɣġ��軰�Թ԰٤Τߡ�
           03  PTBYO-KOHID             PIC 9(10).
      *�쥻�ץ�ɽ���ե饰
           03  PTBYO-REZEFLG           PIC X(01).
      *�쥻�ץ�ɽ�����
           03  PTBYO-REZEMM            PIC 9(02).
      *ž����ʬ
           03  PTBYO-TENKIKBN          PIC X(01).
      *ž����
           03  PTBYO-TENKIYMD          PIC X(08).
      *��̾
           03  PTBYO-BYOMEI            PIC X(160).
      *��̾ʸ����
           03  PTBYO-BYOMEIMOJI        PIC 9(02).
      *�������̾
           03  PTBYO-CHARTBYOMEI       PIC X(160).
      *�������̾ʸ����
           03  PTBYO-CHARTBYOMEIMOJI   PIC 9(02).
      *��­������
           03  PTBYO-HOSOKU-COMMENT    PIC X(80).
      *��­�����ȥ�����
           03  PTBYO-HOSOKUCD-G.
               05  PTBYO-HOSOKUCD      PIC X(20)
                                       OCCURS   3.
      *��̾�Խ��ե饰
           03  PTBYO-BYOMEIHENFLG      PIC X(01).
      *�ݸ���̾�ե饰
           03  PTBYO-HKNBYOMEIFLG      PIC X(01).
      *�쥻�ť��顼�ե饰
           03  PTBYO-RECEDENFLG        PIC X(01).
      *�ݸ��ֹ�
           03  PTBYO-HKNNUM            PIC X(03).
      *�������̥�����
           03  PTBYO-KHNBUICD-G.
               05  PTBYO-KHNBUICD      PIC X(07)
                                       OCCURS  3.
      *���Ͻ�Ϣ��
           03  PTBYO-SEQNUM            PIC 9(04).
      *�ౡ�����񵭺ܥե饰
           03  PTBYO-DISCHARGEFLG      PIC X(01).
      *��������ʬ
           03  PTBYO-CLASS1            PIC 9(02).
      *��ʻ�ɶ�ʬ
           03  PTBYO-CLASS2            PIC 9(02).
      *����ե饰
           03  PTBYO-DLTFLG            PIC X(01).
      *������ڥ졼���ɣ�
           03  PTBYO-DLT-OPID          PIC X(16).
      *ü���ɣ�
           03  PTBYO-TERMID            PIC X(16).
      *���ڥ졼���ɣ�
           03  PTBYO-OPID              PIC X(16).
      *����ǯ����
           03  PTBYO-CREYMD            PIC X(08).
      *��������
           03  PTBYO-CREHMS            PIC X(06).
      *����ǯ����
           03  PTBYO-UPYMD             PIC X(08).
      *��������
           03  PTBYO-UPHMS             PIC X(06).
      *
      *��̾�����ѹ���
           03  PTBYO-XXBYOMEI         PIC X(160).