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
      *         �����ƥ�����ޥ���     (CPSK1101.INC)   *
      *         �ʣ���������                            *
      *         �ǡ��������å���ǽ����                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-1101-REC.
           03  SYS-1101-KEY.
               05  SYS-1101-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1101-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1101-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1101-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1101-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1101-TBL.
      *        ��̾�����å��ѥ�����
               05  SYS-1101-BYOCHKPTN-G.
                   07  SYS-1101-BYOCHKPTN-OCC  OCCURS 5.
                       09  SYS-1101-BYOCHKPTN
                                        PIC  X(1).
               05  SYS-1101-PRTSRTKBN   PIC  X(1).
               05  SYS-1101-BYOSRYKAKBN PIC  X(1).
               05  SYS-1101-PRTPTLSTKBN PIC  X(1).
               05  SYS-1101-UTAGAIKKN-X PIC  X(3).
               05  SYS-1101-UTAGAIKKN   REDEFINES  SYS-1101-UTAGAIKKN-X
                                        PIC  9(03).
               05  SYS-1101-YBUTAGAIKBN PIC  X(1).
               05  SYS-1101-KYUSEIKKN-X PIC  X(3).
               05  SYS-1101-KYUSEIKKN   REDEFINES  SYS-1101-KYUSEIKKN-X
                                        PIC  9(03).
               05  SYS-1101-SYUBYOCHKKBN PIC  X(1).
               05  SYS-1101-YBKENSAYAKUKBN PIC  X(1).
               05  SYS-1101-TENKITEKBYOKBN PIC  X(1).
               05  SYS-1101-SRYHEIWRNKBN   PIC  X(1).
               05  SYS-1101-REIGAICHKKBN   PIC  X(1).
               05  SYS-1101-SEIHOJNUMCHKKBN PIC  X(1).
               05  SYS-1101-SYA-INGAITEN    PIC 9(04).
               05  SYS-1101-KOK-INGAITEN    PIC 9(04).
               05  SYS-1101-KOU-INGAITEN    PIC 9(04).
               05  SYS-1101-RSI-INGAITEN    PIC 9(04).
               05  SYS-1101-JBI-INGAITEN    PIC 9(04).
               05  SYS-1101-JIH-INGAITEN    PIC 9(04).
               05  SYS-1101-KGI-INGAITEN    PIC 9(04).
               05  SYS-1101-CKN-INGAITEN    PIC 9(04).
               05  SYS-1101-TEKBYOYKZKBN    PIC  X(1).
               05  SYS-1101-TEKBYOSRYKBN    PIC  X(1).
               05  SYS-1101-COMPBYONISSUKBN PIC  X(1).
               05  SYS-1101-YBSJTYKZKBN     PIC  X(1).
               05  SYS-1101-YBMSIYKZKBN     PIC  X(1).
               05  SYS-1101-YBSHOHOYKZKBN   PIC  X(1).
               05  SYS-1101-SRYKAISUMSTKBN  PIC  X(1).
               05  SYS-1101-SRYKAISUWRNKBN  PIC  X(1).
               05  SYS-1101-SRYHEIMSTKBN   PIC  X(1).
               05  SYS-1101-SRYHEIEXPKBN PIC  X(1).
               05  SYS-1101-YKZHEIMSTKBN   PIC  X(1).
               05  SYS-1101-SRYSTTENKIKBN   PIC  X(1).
               05  SYS-1101-SRYMATERKKBN    PIC  X(1).
               05  SYS-1101-INGAIPROCKBN    PIC  X(1).
               05  SYS-1101-YOBI            PIC  X(433).
      *ü���ɣ�
           03  SYS-1101-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1101-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1101-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1101-UPYMD           PIC  X(08).
      *��������
           03  SYS-1101-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1101-UPD-KEY.
               05  SYS-1101-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1101-UPD-KANRICD     PIC X(04).
               05  SYS-1101-UPD-KBNCD       PIC X(08).
               05  SYS-1101-UPD-STYUKYMD    PIC X(08).
               05  SYS-1101-UPD-EDYUKYMD    PIC X(08).