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
      *-------------------------------------------------*
      *         �����ƥ�����ޥ���     (CPSK1008.INC)   *
      *         �ʣ���������                            *
      *         �����å���ǽ�������                    *
      *-------------------------------------------------*
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  02/10/10  ��������û������å��ɲ�
      *  01.00.02    NACL-¿��Ǽ  03/10/16  ������ܲû���ư���꤫��
      *                                     ���襫�������ư�ɲ�
      *  01.00.03    NACL-¿��Ǽ  04/01/09  ���ޤȤ������ɲ�
      *  01.00.04    NACL-¿��Ǽ  04/01/15  ���̥��������ɲ�
      *  01.00.05    NACL-¿��Ǽ  04/03/08  ���ֳ��û��������Ļ�
      *                                     �������Ǵ����û����ʼ̿����ǡ��ɲ�
      *  01.00.06    NACL-¿��Ǽ  04/07/05  �ǽ������������Ǥδ����ɲ�
      *  01.00.07    NACL-¿��Ǽ  04/12/06  ���Լ�ó�ǧ�ɲ�
      *  01.00.08    NACL-¿��Ǽ  05/06/06  ���ӽ�λŸ�����μ�ưȯ��
      *                                     ���޾������������å��ɲ�
      *  01.00.09    NACL-¿��Ǽ  05/07/06  �������μ�ưȯ���ɲ�
      *  01.00.10    NACL-¿��Ǽ  05/12/21  ��̾ɽ����ʬ
      *                                     ����Ͽ���ʬ�ɲ�
      *  01.00.11    NACL-¿��Ǽ  06/03/14  ���θ����û�
      *  04.00.00    NACL-¿��Ǽ  07/09/06  �����ݸ��ȹ礻�������å��ɲ�
      *  04.01.00    NACL-¿��Ǽ  07/12/XX  �������μ�ưȯ�����������ɲ�
      *                                     ��̾����μ�ưȯ���ɲ�
      *  04.03.00    NACL-¿��Ǽ  08/07/24  ���������û��μ�ưȯ��
      *  04.05.00    NACL-¿��Ǽ  09/10/16  �ǻ����μ�ưȯ��
      *  04.08.00    NACL-¿��Ǽ  16/08/04  ��ߺ��ѥ����å���ʬ�ɲ�
      *  04.08.00    NACL-¿��Ǽ  16/11/XX  ������Ϳ�̥����å���ʬ�ɲ�
      *  04.08.00    NACL-¿��Ǽ  17/03/XX  �������μ�ưȯ���ʽ��������ɲ�
      *
      *****************************************************************
       01  SYS-1008-REC.
           03  SYS-1008-KEY.
               05  SYS-1008-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1008-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1008-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1008-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1008-EDYUKYMD    PIC  X(08).
      *��ߺ��ѥ����å�����
           03  SYS-1008-INTERACT-KIKAN  PIC 9(02).
      *��������û������å�
           03  SYS-1008-GAIKANRIKBN-CHK PIC X(01).
      *������ܲû���ư����
           03  SYS-1008-IKUJIEIYO-FLG   PIC X(01).
      *������ܲû���ư�����
           03  SYS-1008-IKUJIEIYO-SRYKA PIC X(02).
      *���ֳ��û��������Ļ�
           03  SYS-1008-JIKANTOKU-NYU-FLG  PIC X(01).
      *�������Ǵ����û����ʼ̿����ǡ�
           03  SYS-1008-GAZOUKSN1-FLG      PIC X(01).
      *���ӽ�λŸ�����μ�ưȯ��
           03  SYS-1008-CHUTOTENKAI-FLG    PIC X(01).
      *ͽ��
      *****03  FILLER                   PIC X(04).
           03  SYS-1008-YOBI1           PIC X(01).
      *���襫�������ư
           03  SYS-1008-GAI-CURSOR-SET-1   PIC X(01).
      *�����������ưȯ��
           03  SYS-1008-NYU-SYOSIN      PIC X(01).
      *
      *���ޤȤ�����
           03  SYS-1008-HOUKATU-FLG     PIC X(01).
      *
      *���̥�������
           03  SYS-1008-SURYOZERO-FLG   PIC X(01).
      *����Ĵ�޵��Ѵ����������å�
           03  SYS-1008-CHOKICHK-FLG   PIC X(01).
      *���ֳ��û����Ļ������å�
           03  SYS-1008-JIKANTOKU-CHK      PIC X(01).
      *�����å���������
      *    ʿ��
           03  SYS-1008-JIKANCHK-WEEK-G.
               05  SYS-1008-JIKANCHK-WEEK      OCCURS   2.
                   07  SYS-1008-JIKANCHK-WST   PIC 9(04).
                   07  SYS-1008-JIKANCHK-WED   PIC 9(04).
      *    ����
           03  SYS-1008-JIKANCHK-SAT-G.
               05  SYS-1008-JIKANCHK-SAT       OCCURS   2.
                   07  SYS-1008-JIKANCHK-SST   PIC 9(04).
                   07  SYS-1008-JIKANCHK-SED   PIC 9(04).
      *�ǽ�������������Ƚ��ޤǤδ���
           03  SYS-1008-LAST-KIKAN-X.
               05  SYS-1008-LAST-KIKAN       PIC 9(02).
      *���Ź԰ٴ��Լ�ó�ǧ�����å�
           03  SYS-1008-CLEARCHK-FLG   PIC X(01).
      *���޾������������å�
           03  SYS-1008-YAKJYOCHK-FLG   PIC X(01).
      *�������μ�ưȯ���ʳ����
           03  SYS-1008-TEISEIAUTO-FLG         PIC X(01).
      *�������μ�ưȯ���������������������
           03  SYS-1008-TEISEITOKU-FLG         PIC X(01).
      *��̾ɽ����ʬ
           03  SYS-1008-BYOHYOKBN-FLG          PIC X(01).
      *����Ͽ���ʬ
           03  SYS-1008-ZAIJUNKBN-FLG          PIC X(01).
      *���θ����û���ưȯ��
           03  SYS-1008-KENSAAUTO-FLG          PIC X(01).
      *�����ݸ��ȹ礻�������å���ʬ
           03  SYS-1008-ZENHKNCHK-FLG          PIC X(01).
      *�������μ�ưȯ����������
           03  SYS-1008-TEINYUAUTO-FLG         PIC X(01).
      *��̾������ʬ����μ�ưȯ��
           03  SYS-1008-BYOMEIAUTO-FLG         PIC X(01).
      *���������û��μ�ưȯ��
           03  SYS-1008-NYUKENSA-FLG    PIC X(01).
      *�ǻ����μ�ưȯ��
           03  SYS-1008-INITAUTO-FLG    PIC X(01).
      *��Ģ�û���ǧ�����å�
           03  SYS-1008-TECHOKBN-FLG        PIC X(01).
      *���ֳ��۵ޱ�������å�
      *     ��ver.4.7����YOBI2�Σ����ܡ�
           03  SYS-1008-JGAIKEIKBN-FLG      PIC X(01).
      *��ߺ��ѥ����å���ʬ
           03  SYS-1008-INTERACT-CHKKBN     PIC X(01).
      *������Ϳ�̥����å���ʬ
           03  SYS-1008-TOUYOCHK-FLG        PIC X(01).
      *��������Ÿ���ʽ�������
           03  SYS-1008-TEISHOHOU-FLG       PIC X(01).
      *����������Ķ�����å���ʬ
           03  SYS-1008-SHOHOU30CHK         PIC X(01).
      *ͽ��
      *****03  FILLER                   PIC X(489).
      *****03  FILLER                   PIC X(486).
      **** 03  FILLER                   PIC X(453).
      *****03  FILLER                   PIC X(451).
      *****03  FILLER                   PIC X(450).
      *****03  FILLER                   PIC X(449).
      **** 03  FILLER                   PIC X(445).
      **** 03  SYS-1008-YOBI2           PIC X(440).
      ***  03  SYS-1008-YOBI2           PIC X(436).
           03  SYS-1008-YOBI2           PIC X(435).
      *ü���ɣ�
           03  SYS-1008-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1008-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1008-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1008-UPYMD           PIC  X(08).
      *��������
           03  SYS-1008-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1008-UPD-KEY.
               05  SYS-1008-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1008-UPD-KANRICD     PIC X(04).
               05  SYS-1008-UPD-KBNCD       PIC X(08).
               05  SYS-1008-UPD-STYUKYMD    PIC X(08).
               05  SYS-1008-UPD-EDYUKYMD    PIC X(08).