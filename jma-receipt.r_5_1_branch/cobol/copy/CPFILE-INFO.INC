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
      *    �ե��������ģ�              (CPFILE-INFO.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  FILE-INFO-REC.
      *
          03  FILE-INFO-KEY.
      *
               05  FILE-INFO-HOSPNUM   PIC 9(2).
      *�ˣţ�
               05  FILE-INFO-TBL-KEY   PIC X(08).
      *����ǯ��
               05  FILE-INFO-SRYYM     PIC X(06).
      *������ɣ�
               05  FILE-INFO-SHELLID   PIC X(20).
      *Ϣ��
               05  FILE-INFO-RENNUM    PIC 9(03).
      *������Ϣ��
               05  FILE-INFO-SHORI-RENNUM
                                       PIC 9(03).
      *����ǯ����
           03  FILE-INFO-SKYYMD        PIC X(08).
      *�����ȥ�
           03  FILE-INFO-TITLE         PIC X(100).
      *������ʧ����
           03  FILE-INFO-TEISYUTUSAKI  PIC X(01).
      *������ʬ
           03  FILE-INFO-NYUGAIKBN     PIC X(01).
      *���
           03  FILE-INFO-CNT-ALL       PIC 9(07).
      *�����������
           03  FILE-INFO-CNT-NYUIN     PIC 9(07).
      *�������������
           03  FILE-INFO-CNT-GAIRAI    PIC 9(07).
      *����Ǽ���
           03  FILE-INFO-FOR-FOLDER    PIC X(200).
      *���ե�����̾
           03  FILE-INFO-FOR-DATA      PIC X(200).
      *���Ǽ���
           03  FILE-INFO-TO-FOLDER     PIC X(200).
      *��ե�����̾
           03  FILE-INFO-TO-DATA       PIC X(200).
      *�����ɶ�ʬ�ʶ��򡢣����ӡݣ��� �����ţգá�
           03  FILE-INFO-CODE-TYPE     PIC X(01).
      *�ãӣ��ѣģ�̵ͭ��ʬ�ʣ����ģ�ͭ��
           03  FILE-INFO-DATA-TYPE     PIC X(01).
      *���顼�����ɡʣ������顼ͭ��
           03  FILE-INFO-ERR-CODE      PIC X(01).
      *���顼��å�����
           03  FILE-INFO-ERR-MSG       PIC X(200).
      *������
           03  FILE-INFO-SYOYMD        PIC X(08).
      *������
           03  FILE-INFO-CREYMD        PIC X(08).
      *��������
           03  FILE-INFO-CREHMS        PIC X(06).
