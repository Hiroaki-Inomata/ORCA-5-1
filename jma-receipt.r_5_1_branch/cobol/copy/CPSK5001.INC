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
      *         �����ƥ�����ޥ���     (CPSK5001.INC)   *
      *         �ʣ���������                            *
      *         ����ޥ���                              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-5001-REC.
           03  SYS-5001-KEY.
               05  SYS-5001-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-5001-KANRICD            PIC  X(04).
      *��ʬ������
               05  SYS-5001-KBNCD              PIC  X(08).
      *ͭ������ǯ����
               05  SYS-5001-STYUKYMD           PIC  X(08).
      *ͭ����λǯ����
               05  SYS-5001-EDYUKYMD           PIC  X(08).
      *�����ơ��֥�
           03  SYS-5001-TBL.
      *�����ֹ�
               05  SYS-5001-BTU-NUM            PIC  X(02).
      *����̾��
               05  SYS-5001-BTU-NAME           PIC  X(20).
      *����û��̾��
               05  SYS-5001-BTU-TANNAME        PIC  X(10).
      *����������
               05  SYS-5001-BTU-TOKTEI-NYUIN
                                               PIC  X(02).
      *�����
               05  SYS-5001-BTU-KA             PIC  X(02).
      *�������
               05  SYS-5001-BTU-SBT
                                               PIC  X(02).
      *���ƿͿ�
               05  SYS-5001-BTU-NINZU          PIC  9(03).
      *ʿ�Ѻ߱�������ʬ
               05  SYS-5001-BTU-HEIKINZAIKBN
                                               PIC  X(02).
      *�Ǹ����ֶ�ʬ
               05  SYS-5001-BTU-KNGHAICHIKBN
                                               PIC  X(02).
      *�Ǹ����Ψ��ʬ
               05  SYS-5001-BTU-KNGHIRITUKBN
                                               PIC  X(02).
      *�Ǹ������Ψ��ʬ
               05  SYS-5001-BTU-KNGHJOHAI
                                               PIC  X(02).
      *�Ǹ�����û���ʬ
               05  SYS-5001-BTU-KNGHJOKSN
                                               PIC  X(02).
      *��ֶ�̳���Ǹ�û���ʬ
               05  SYS-5001-BTU-YAKANKNGKSN
                                               PIC  X(02).
      *��Ʋ�û���ʬ
               05  SYS-5001-BTU-SYOKUDOKBN
                                               PIC  X(01).
      *�Ǹ�տͿ���ʬ
               05  SYS-5001-BTU-KNGSISUKBN
                                               PIC  X(01).
      *�ݰ�οͿ���ʬ
               05  SYS-5001-BTU-HIKSISUKBN
                                               PIC  X(01).
      *��������������������
               05  SYS-5001-BTU-KHNSRYCD
                                               PIC  X(09).
      *����û�����������
               05  SYS-5001-BTU-KSNSRYCD
                                               PIC  X(09).
      *Ĺ����������������
               05  SYS-5001-BTU-GENSRYCD
                                               PIC  X(09).
      *���ｪλ��ʬ
               05  SYS-5001-BTU-EDKBN
                                               PIC  X(01).
      *�û��ΰ�
               05  SYS-5001-KSNAREA.
      *���´����������������Ųû�
                 07  SYS-5001-NANBYO.
                   09  SYS-5001-NANBYO-CD      PIC X(09).
                   09  SYS-5001-NANBYO-KBN     PIC X(01).
      *�ü�����������ߴ����û�
                 07  SYS-5001-TOKUSYU.
                   09  SYS-5001-TOKUSYU-CD     PIC X(09).
                   09  SYS-5001-TOKUSYU-KBN    PIC X(01).
      *�����������ܴĶ��û�
      *���¼���������˰�ư H15.02.27)
                 07  SYS-5001-YORYO              OCCURS  3.
                   09  SYS-5001-YORYO-CD       PIC X(09).
                   09  SYS-5001-YORYO-KBN      PIC X(01).
      *��Ƹ���׽մ��������������Ŵ����û�
                 07  SYS-5001-JIDO.
                   09  SYS-5001-JIDO-CD        PIC X(09).
                   09  SYS-5001-JIDO-KBN       PIC X(01).
      *�����ʱ����������ߴ����û�
                 07  SYS-5001-SEISIN.
                   09  SYS-5001-SEISIN-CD      PIC X(09).
                   09  SYS-5001-SEISIN-KBN     PIC X(01).
      *�Ǹ����ֲû�
                 07  SYS-5001-KANGO              OCCURS  5.
                   09  SYS-5001-KANGO-CD       PIC X(09).
                   09  SYS-5001-KANGO-KBN      PIC X(01).
      *�Ǹ�����Ŭ��û�
                 07  SYS-5001-KNGTEK.
                   09  SYS-5001-KNGTEK-CD      PIC X(09).
                   09  SYS-5001-KNGTEK-KBN     PIC X(01).
      *ʣ����յڤӴǸ�����Ŭ��û�
                 07  SYS-5001-DRTEK.
                   09  SYS-5001-DRTEK-CD       PIC X(09).
                   09  SYS-5001-DRTEK-KBN      PIC X(01).
      *�������û������
      *ʿ������ǯ����ʹߤϻ��ꤹ��û������ɤ�����Σ����ܤ��
      *�缡���ꤵ��롣
               05  SYS-5001-KSNTBLR  REDEFINES SYS-5001-KSNAREA.
                 07  SYS-5001-KSN              OCCURS  14.
                   09  SYS-5001-KSNCD          PIC X(09).
                   09  SYS-5001-KSNKBN         PIC X(01).
      *�������û��ʥ桼�������ΰ��
               05  SYS-5001-KSN2.
                 09  SYS-5001-KSNCD2           PIC X(09) OCCURS 10.
      *ͽ�� ** ͽ���θ�ˤ���ܤ�����Τ����դ��뤳�� **
               05  SYS-5001-YOBI               PIC X(130).
      *�Ǹ����Ψ�����ֶ�ʬ��ʿ������ǯ�����������������������ηв����֡�
               05  SYS-5001-SYOKUINKBN2    PIC  X(01).
      *�žɴ��Գ������������������ʬ��ʿ������ǯ��������������
               05  SYS-5001-JUUSHOUKNGKBN  PIC  X(01).
      *������Ź԰٥����ɡ�������������
               05  SYS-5001-TSRYCD         PIC  X(09).
      *������Ź԰٥�����
               05  SYS-5001-SRYCD          PIC  X(09).
      *�Ǹ�������в����ֶ�ʬ
               05  SYS-5001-SYOKUINKBN     PIC  X(01).
      *��л�������������������ʬ
               05  SYS-5001-YKNTOKUKBN     PIC  X(01).
      *��ʿ����л���Ķ�Ḻ��
               05  SYS-5001-YKNCHOKAKBN    PIC  X(01).
      *���Ž���̡������¾���߻����ʬ
      *      ������̡�08��ͭ�����̡�
      *      ������߻��ꤷ�ʤ�
      *        ������߻��ꤷ�ʤ�
      *        ������߻��ꤹ��
      *      ������̡�09��ͭ�����ܡ�
      *      ������߻��ꤷ�ʤ�
      *        ������߻��ꤷ�ʤ�
      *        ������߻��ꤹ���ͭ�����Ž���������������
      *        ������߻��ꤹ���ͭ�����Ž���������������
      *        ������߻��ꤹ���ͭ�����Ž���������������
               05  SYS-5001-CRYOYOKBN          PIC X(01).
      *�����������Ķ���� *      ����������������������
      *        ����������������������
      *        ����������������������������ˤ�껻��
               05  SYS-5001-HRYOYOKBN          PIC X(01).
               05  SYS-5001-KHNSRYCD           PIC X(09).
               05  SYS-5001-KHNKBNCD           PIC X(03).
      *����������������ʰ��̡�
               05  SYS-5001-SENTEI-IPN-KINGAKU-X
                                               PIC  X(04).
               05  SYS-5001-SENTEI-IPN-KINGAKU-R
                                           REDEFINES
                   SYS-5001-SENTEI-IPN-KINGAKU-X.
                   07  SYS-5001-SENTEI-IPN-KINGAKU
                                               PIC  9(04).
      *�����������������Ϸ�͡�
               05  SYS-5001-SENTEI-RJN-KINGAKU-X
                                               PIC  X(04).
               05  SYS-5001-SENTEI-RJN-KINGAKU-R
                                           REDEFINES
                   SYS-5001-SENTEI-RJN-KINGAKU-X.
                   07  SYS-5001-SENTEI-RJN-KINGAKU
                                               PIC  9(04).
      *�������������ʬ
               05  SYS-5001-BTU-ISISUKBN       PIC  X(01).
      *�������������ʬ
               05  SYS-5001-SENTEI-FTNKBN      PIC  X(01).
      *�������������Ƕ�ʬ
               05  SYS-5001-SENTEI-TAXKBN      PIC  X(01).
      *������������ñ��
               05  SYS-5001-SENTEI-TANKA-X     PIC  X(02).
               05  SYS-5001-SENTEI-TANKA-R     REDEFINES
                   SYS-5001-SENTEI-TANKA-X.
                   07  SYS-5001-SENTEI-TANKA   PIC  9(02).
      *����������������ʶ��̡�
               05  SYS-5001-SENTEI-KINGAKU-X   PIC  X(04).
               05  SYS-5001-SENTEI-KINGAKU-R   REDEFINES
                   SYS-5001-SENTEI-KINGAKU-X.
                   07  SYS-5001-SENTEI-KINGAKU PIC  9(04).
      *�쥻�ץ�ɽ����ʬ
               05  SYS-5001-RECEDSPKBN         PIC  X(02).
      *ü���ɣ�
           03  SYS-5001-TERMID                 PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-5001-OPID                   PIC  X(16).
      *����ǯ����
           03  SYS-5001-CREYMD                 PIC  X(08).
      *����ǯ����
           03  SYS-5001-UPYMD                  PIC  X(08).
      *��������
           03  SYS-5001-UPHMS                  PIC  X(06).
      *�祭�������ѹ���
           03  SYS-5001-UPD-KEY.
               05  SYS-5001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5001-UPD-KANRICD     PIC X(04).
               05  SYS-5001-UPD-KBNCD       PIC X(08).
               05  SYS-5001-UPD-STYUKYMD    PIC X(08).
               05  SYS-5001-UPD-EDYUKYMD    PIC X(08).