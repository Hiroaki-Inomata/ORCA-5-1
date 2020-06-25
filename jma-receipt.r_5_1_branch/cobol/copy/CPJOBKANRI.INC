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
      *         ����ִ���    COPY   (CPJOBKANRI.INC)            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02.08.30  ���ƥå׷�����ɲ�
      *  02.05.00    NACL-����    04.09.27  JOB��ߥե饰���ɲ�
      *  02.06.00    NACL-����    05.08.29  ����ǯ�����ɲ�
      *
      *  04.07.01    NACL-ƣ��    12.09.07  ��������ʸ��̡ˤ��ɲ�
      *
      *  05.01.01    NACL-ƣ��    17.07.07  ����ƣգɣġ����륫�գɣĤ��ɲ�
      ************************************************************
      *01  JOBKANRI-REC.
      *����
           03  JOB-KEY.
               05  JOB-HOSPNUM           PIC 9(2).
      *����ִ���������
               05  JOB-JOBID            PIC  9(07).
      *������ɣ�
               05  JOB-SHELLID          PIC  X(08).
      *�»ܥ��ڡ��졼���ɣ�
               05  JOB-RUN-OPID         PIC  X(16).
      *������ͣӣ�
           03  JOB-SHELLMSG             PIC  X(100).
      *ü���ɣ�
           03  JOB-TERMID               PIC  X(32).
      *���ڡ��졼���ɣ�
           03  JOB-OPID                 PIC  X(16).
      *���ڡ��졼���ɣ�
           03  JOB-PGID                 PIC  X(20).
      *����ֳ���ǯ����
           03  JOB-STARTYMD             PIC  X(08).
      *����ֽ�λǯ����
           03  JOB-ENDYMD               PIC  X(08).
      *����ֳ��ϻ���
           03  JOB-STARTTIME            PIC  X(08).
      *����ֽ�λ����
           03  JOB-ENDTIME              PIC  X(08).
      *���ƥå׳��ϻ���
           03  JOB-STEPSTARTTIME        PIC  X(08).
      *���ƥå׽�λ����
           03  JOB-STEPENDTIME          PIC  X(08).
      *�ѥ�᡼��
           03  JOB-PARAM                PIC  X(256).
      *���顼������
           03  JOB-ERRCD                PIC  X(04).
      *��硦���̶�ʬ
           03  JOB-SYOKBN               PIC  X(01).
      *����ǯ��
           03  JOB-SRYYM                PIC  X(06).
      *������ʬ
           03  JOB-NYUGAIKBN            PIC  X(01).
      *����ǯ����
           03  JOB-SRYYMD               PIC  X(08).
      *���ԣɣ�
           03  JOB-PTID                 PIC  9(10).
      *�������
           03  JOB-UPDCNT               PIC  9(10).
      *���ƥå׷��
           03  JOB-STEPCNT              PIC  9(03).
      *������ߥե饰
           03  JOB-STOPFLG              PIC  9(01).
      *�ץ�����ID
           03  JOB-PID1                 PIC  9(05).
           03  JOB-PID2                 PIC  9(05).
           03  JOB-PID3                 PIC  9(05).
           03  JOB-PID4                 PIC  9(05).
      *��������ʸ��̡�
           03  JOB-UPDCNT1              PIC  9(10).
           03  JOB-UPDCNT2              PIC  9(10).
      *����ƣգɣ�
           03  JOB-KARTE-UID            PIC X(36).
      *���륫�գɣ�
           03  JOB-ORCA-UID             PIC X(36).
      *ͽ��
           03  JOB-YOBI                 PIC  X(500).