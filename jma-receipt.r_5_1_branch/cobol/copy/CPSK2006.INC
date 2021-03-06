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
      *         �����ƥ�����ޥ���     (CPSK2006.INC)            *
      *         �ʣ���������                                     *
      *         �쥻�ץ��õ������Խ�����                         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.07.01    NACL-ƣ��    14/02/10  ������ʬ��������ʬ���ɲ�
      ************************************************************
       01  SYS-2006-REC.
           03  SYS-2006-KEY.
               05  SYS-2006-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2006-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2006-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2006-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2006-EDYUKYMD    PIC  X(08).
      *���ݸ��ֹ�
           03  SYS-2006-HKNNUM          PIC  X(03).
      *��������ʧ����
           03  SYS-2006-TEISYUTUSAKI    PIC  X(01).
      *���ݸ����ֹ�
           03  SYS-2006-HKNJANUM        PIC  X(08).
      *��Ϸ�Ͷ�ʬ
           03  SYS-2006-RJNKBN          PIC  X(01).
      *�������Ƕ�ʬ
           03  SYS-2006-ZAITAKU         PIC  X(01).
      *�õ�����
           03  SYS-2006-TOKKI           PIC  X(24).
      *����ô���ֹ�
           03  SYS-2006-FTNJANUMKBN-JYOKEN
                                        PIC  X(01).
      *����ô���ֹ�����
           03  SYS-2006-FTNJANUM-JYOKEN PIC  X(08).
      *��������ʬ
           03  SYS-2006-NYUGAIKBN       PIC  X(01).
      *��������ʬ
           03  SYS-2006-SYOTOKUKBN      PIC  X(01).
      *ͽ��
           03  SYS-2006-YOBI1           PIC  X(89).
      *���ճ��
           03  SYS-2006-KYURATE         PIC  9(03).
      *ͽ��
           03  SYS-2006-YOBI2           PIC  X(02).
      *�쥻�š��õ�����
           03  SYS-2006-RECEDEN-TOKKI   PIC  X(02).
      *�쥻�š����ճ��
           03  SYS-2006-RECEDEN-KYURATE PIC  9(03).
      *�쥻�š�����쥳���ɶ�ʬ
           03  SYS-2006-RECEDEN-KORECKBN
                                        PIC  X(01).
      *Ŭ������ʥ�����
           03  SYS-2006-TEKIYOU-COMMENT1
                                        PIC  X(52).
      *Ŭ������ʥ����ȥ쥻���Խ���ʬ
           03  SYS-2006-TEKIYOU-COMMENT1KBN
                                        PIC  X(01).
      *Ŭ�����ʥ�����
           03  SYS-2006-TEKIYOU-COMMENT2
                                        PIC  X(52).
      *Ŭ�����ʥ����ȥ쥻���Խ���ʬ
           03  SYS-2006-TEKIYOU-COMMENT2KBN
                                        PIC  X(01).
      *ͽ��
           03  SYS-2006-YOBI3           PIC  X(49).
      *��ô���ֹ��Խ���ʬ
           03  SYS-2006-FTNJANUMKBN     PIC  X(02).
      *��ô���ֹ��Խ�����
           03  SYS-2006-FTNJANUM        PIC  X(08).
      *������ֹ��Խ���ʬ
           03  SYS-2006-JKYSNUMKBN      PIC  X(02).
      *������ֹ��Խ�����
           03  SYS-2006-JKYSNUM         PIC  X(07).
      *�쥻�š���ô���ֹ��Խ���ʬ
           03  SYS-2006-RECEDEN-FTNJANUMKBN
                                        PIC  X(02).
      *�쥻�š���ô���ֹ��Խ�����
           03  SYS-2006-RECEDEN-FTNJANUM
                                        PIC  X(08).
      *�쥻�š�������ֹ��Խ���ʬ
           03  SYS-2006-RECEDEN-JKYSNUMKBN
                                        PIC  X(02).
      *�쥻�š�������ֹ��Խ�����
           03  SYS-2006-RECEDEN-JKYSNUM PIC  X(07).
      *ͽ��
           03  SYS-2006-YOBI4           PIC  X(158).
      *ü���ɣ�
           03  SYS-2006-TERMID          PIC  X(16).
      *���ڥ졼���ɣ�
           03  SYS-2006-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2006-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2006-UPYMD           PIC  X(08).
      *��������
           03  SYS-2006-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2006-UPD-KEY.
               05  SYS-2006-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2006-UPD-KANRICD     PIC X(04).
               05  SYS-2006-UPD-KBNCD       PIC X(08).
               05  SYS-2006-UPD-STYUKYMD    PIC X(08).
               05  SYS-2006-UPD-EDYUKYMD    PIC X(08).
