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
      *         �����ƥ�����ޥ���     (CPSK2008.INC)            *
      *         �ʣ���������                                     *
      *         �쥻�ץ����ֽ����                               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-2008-REC.
           03  SYS-2008-KEY.
               05  SYS-2008-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2008-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2008-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2008-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2008-EDYUKYMD    PIC  X(08).
      *��������� 
          03  SYS-2008-PRTJYUN.
      *        ���⸩��
               05  SYS-2008-PREFKBN-PRTJYUN
                                        PIC  X(01).
      *        �ݸ���
               05  SYS-2008-HKNJANUM-PRTJYUN
                                        PIC  X(01).
      *        �쥻�ץȼ���
               05  SYS-2008-RECESYUBETU-PRTJYUN
                                        PIC  X(01).
      *�����ǡ��߰���
           03  SYS-2008-ZAITAKU         PIC  X(01).
      *Ĺ����Ĺ������������
           03  SYS-2008-CHOUKI          PIC  X(01).
      *���ŵ��ؤν�����ݸ���
           03  SYS-2008-HKNJANUM        PIC  X(08).
      *���ŵ��ؤν�����ݸ��԰�������
           03  SYS-2008-HKNJANUM-PRTKBN PIC  X(01).
      *ͽ��
           03  SYS-2008-YOBI            PIC  X(486).
      *ü���ɣ�
           03  SYS-2008-TERMID          PIC  X(16).
      *���ڥ졼���ɣ�
           03  SYS-2008-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2008-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2008-UPYMD           PIC  X(08).
      *��������
           03  SYS-2008-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2008-UPD-KEY.
               05  SYS-2008-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2008-UPD-KANRICD     PIC X(04).
               05  SYS-2008-UPD-KBNCD       PIC X(08).
               05  SYS-2008-UPD-STYUKYMD    PIC X(08).
               05  SYS-2008-UPD-EDYUKYMD    PIC X(08).