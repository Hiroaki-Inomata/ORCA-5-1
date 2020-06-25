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
      *         �����ƥ�����ޥ���     (CPSK9000.INC)            *
      *         �ʣ���������                                     *
      *         �ạ̃��ɣ���³����                               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 03.05.00     NACL-¿��Ǽ  07/07/17  ����ǻ���Ÿ���ɲ�
      ************************************************************
       01  SYS-9000-REC.
           03  SYS-9000-KEY.
               05  SYS-9000-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-9000-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-9000-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-9000-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-9000-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-9000-TBL.
      *��³̵ͭ��ʬ
               05  SYS-9000-SETUZOKU    PIC  X(01).
      *�����ѥ��󥹥��󥹥�����
               05  SYS-9000-CODE        PIC  X(01).
      *���եݥåץ��å�ɽ��
               05  SYS-9000-UKEBUTTON-G.
                 07 SYS-9000-UKEBUTTON  PIC  X(01) OCCURS 2.
      *������Ͽ�ݥåץ��å�ɽ��
               05  SYS-9000-KANBUTTON-G.
                 07 SYS-9000-KANBUTTON  PIC  X(01) OCCURS 2.
      *���Ź԰٥ݥåץ��å�ɽ��
               05  SYS-9000-SYNBUTTON-G.
                 07 SYS-9000-SYNBUTTON  PIC  X(01) OCCURS 2.
      *������Ͽ�����ե饰
           03  SYS-9000-PT-FLG          PIC  X(01).
      *�쥻���Ѵ��ե饰
           03  SYS-9000-RECEDEN-FLG     PIC  X(01).
      *��̾����ե饰
           03  SYS-9000-BYOMEI-SYU-FLG  PIC  X(01).
      *��̾Ʊ���ե饰
           03  SYS-9000-BYOMEI-DOUKI-FLG
                                        PIC  X(01).
      *����������������ե饰
           03  SYS-9000-NYU-ORDER-FLG
                                        PIC  X(01).
      *�ޥ���ۥ��ȥե饰
           03  SYS-9000-MULTIHOST-FLG
                                        PIC  X(01).
      *�������ե饰
           03  SYS-9000-RESEND-FLG
                                        PIC  X(01).
      *����ǻ���Ÿ��
           03  SYS-9000-SINSATU-FLG     PIC  X(01).
      *ͽ��
           03  SYS-9000-YOBI            PIC  X(485).
      *ü���ɣ�
           03  SYS-9000-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-9000-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-9000-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-9000-UPYMD           PIC  X(08).
      *��������
           03  SYS-9000-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-9000-UPD-KEY.
               05  SYS-9000-UPD-HOSPNUM     PIC 9(2).
               05  SYS-9000-UPD-KANRICD     PIC X(04).
               05  SYS-9000-UPD-KBNCD       PIC X(08).
               05  SYS-9000-UPD-STYUKYMD    PIC X(08).
               05  SYS-9000-UPD-EDYUKYMD    PIC X(08).