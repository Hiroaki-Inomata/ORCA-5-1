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
      *    �����쥻�ץȡ������ˡ����ܾ���     COPY (CPRCF200.INC) 
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.04.01    NACL-ƣ��    09/03/10  ���̤ʴط��ˤ���¾�������
      *                                     �������Ȥ��Ƶ������쥻�ץȵ�
      *                                     �ܤ�Ԥʤ��б�
      *  04.05.01    NACL-����    10/02/16  ͽ�������ɲ�
      *  04.05.02    NACL-ƣ��    11/11/22  Ʊ���������б�
      *  04.05.03    NACL-����    12/04/09  ����ά�Τ�ʸ������ĥ
      *  04.07.01    NACL-����    12/10/12  ��������ʿ������ɲ�
      *  04.07.02    NACL-����    14/05/13  Ķ�žɻ��ʼԡ��������Ųû�����
      *                                     ����ե饰�ɲ�
      *  04.08.01    NACL-����    16/04/15  ʿ������ǯ��������б�
      *  04.08.02    NACL-����    18/03/27  ʿ������ǯ��������б�
      *  04.08.03    NACL-����    18/04/06  ʿ������ǯ��������б�
      *
      *  05.00.01    NACL-����    19/07/01  ��ϥӥ�ȯ�����쥻�ץȵ����б�
      *                                     �ʵ�ǽ�ɲá�
      ********************************************************************
      *01  RECE200-REC.
           03  RECE200-KEY.
      *���ŵ����ֹ�
               05  RECE200-HOSPNUM         PIC 9(2).
      *����ǯ��
               05  RECE200-SRYYM           PIC 9(06).
      *������ʬ
               05  RECE200-NYUGAIKBN       PIC X(01).
      *���ԣɣ�
               05  RECE200-PTID            PIC 9(10).
      *�쥻�ץȿ��Ų�
               05  RECE200-RECEKA          PIC X(02).
      *����ɣ�
               05  RECE200-HKNID           PIC 9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE200-TEKSTYMD        PIC X(08). 
      *
      *�쥻�ץȼ���
           03  RECE200-RECESYUBETU         PIC 9(04).
      *
      *�쥻�ץ��о�ǯ��������
           03  RECE200-TEKYMD.
      *Ŭ�ѳ���ǯ����
               05  RECE200-TEKSTYMD-X      PIC X(08). 
      *Ŭ�ѽ�λǯ����
               05  RECE200-TEKEDYMD-X      PIC X(08).
      *
      *���Ծ���
           03  RECE200-PTINF.
      *�����ֹ�  
               05  RECE200-PTNUM           PIC X(20).
      *���ʻ�̾
               05  RECE200-KANANAME        PIC X(50).
      *������̾
               05  RECE200-NAME            PIC X(100).
      *����
               05  RECE200-SEX             PIC X(01).
      *��ǯ����
               05  RECE200-BIRTHDAY        PIC 9(08).
      *��˴��ʬ
               05  RECE200-DEATHKBN        PIC X(01).
      *ǯ��
               05  RECE200-AGE             PIC 9(03).
      *�ƥ��ȴ����ֹ��ʬ
               05  RECE200-TSTPTNUMKBN     PIC X(01).
      *
      *���ݸ�����
           03  RECE200-HKNINF.
      *���ݸ����ݸ��ֹ�
               05  RECE200-HKNNUM          PIC X(03).
      *���ݸ����ݸ����ֹ�
               05  RECE200-HKNJANUM        PIC X(08).
      *���ݸ��ݵ���
               05  RECE200-KIGO            PIC X(80).
      *���ݸ����ֹ�
               05  RECE200-NUM             PIC X(80).
      *
      *���Ųʾ���
           03  RECE200-SRYKAINF            OCCURS  20.
      *���Ų�
               05  RECE200-SRYKA           PIC X(02).
      *
      *�ݸ��ȹ礻�ֹ�
           03  RECE200-HKNCOMBI            PIC 9(04).
      *
      *������
           03  RECE200-JNISSU              PIC 9(03).
      *
      *���Ŷ�ʬ����������
           03  RECE200-SRYKBNINF           OCCURS  21.
      *��������
               05  RECE200-KHNTEN          PIC 9(08).
      *���
               05  RECE200-KAISU           PIC 9(05).
      *����
               05  RECE200-TENSU           PIC 9(08)   OCCURS  2.
      *
      *���׾���
           03  RECE200-SYOUKEIINF-AREA.
               05  RECE200-SYOUKEIINF      OCCURS  3.
      *��������
                   07  RECE200-SYOUKEI     PIC 9(08).
      *
      *��׾���
           03  RECE200-TOTALTENINF-AREA.
               05  RECE200-TOTALTENINF     OCCURS  4.
      *�������
                   07  RECE200-TOTALTEN    PIC 9(08).
      *
      *�������������̾���
           03  RECE200-NYUSYUINF           OCCURS  20.
      *��������������
               05  RECE200-NYUSYUBETU      PIC X(20).
      *
      *��������������
           03  RECE200-NYUKHNINF           OCCURS  20.
      *�������������û�
               05  RECE200-NYUKHNRYO       PIC 9(09).
      *��������
               05  RECE200-NYUKHNNISSU     PIC 9(02).
      *��׶��
               05  RECE200-NYUKHNKEI       PIC 9(09).
      *
      *��������������
           03  RECE200-NYUKHNINF2.
      *��׶�ۣ�
               05  RECE200-NYUKHNKEI2      PIC S9(09).
      *
      *������������������
           03  RECE200-NYUTOKINF           OCCURS  20.
      *����������������¾������
               05  RECE200-NYUTOKRYO       PIC 9(09).
      *����������������¾��ά�Ρ�
               05  RECE200-NYUTOKRYO-RYAKU PIC X(20).
      *��������
               05  RECE200-NYUTOKNISSU     PIC 9(02).
      *��׶��
               05  RECE200-NYUTOKKEI       PIC 9(09).
      *
      *������������������
           03  RECE200-NYUTOKINF2.
      *��׶�ۣ�
               05  RECE200-NYUTOKKEI2      PIC S9(09).
      *
      *�����������ܻ�Ƴ
           03  RECE200-KOGAISIDOINF        OCCURS   3.
      *������
               05  RECE200-KOGAISIDORYO    PIC 9(09).
      *����
               05  RECE200-KOGAISIDONISSU  PIC 9(02).
      *��׶��
               05  RECE200-KOGAISIDOKEI    PIC 9(09).
      *
      *����������������
           03  RECE200-SEIKANINF.
      *������
               05  RECE200-SEIKANRYO       PIC 9(09).
      *����
               05  RECE200-SEIKANNISSU     PIC 9(02).
      *��׶��
               05  RECE200-SEIKANKEI       PIC 9(09).
      *
      *�������̾��󡾴���
           03  RECE200-SHOKUJISYUBETU-KHN  PIC X(02).
      *
      *�������̾���
           03  RECE200-SHOKUJISYUINF       OCCURS   3.
      *��������
               05  RECE200-SHOKUJISYUBETU  PIC X(04).
      *
      *��������������
           03  RECE200-SHOKUJIKHNINF       OCCURS   4.
      *����������
               05  RECE200-SHOKUJIKHNRYO   PIC 9(09).
      *��������
               05  RECE200-SHOKUJIKHNNISSU PIC 9(02).
      *
      *��������ʿ�����
           03  RECE200-SHOKUJITOTALDAY     PIC 9(02).
      *
      *�����������ζ�۽���
           03  RECE200-SHOKUJIKHNRYO-KEI   PIC 9(09).
      *
      *¾�ݸ�����������Ѥߥե饰
           03  RECE200-NYUINRYOFLG         PIC X(01).
      *
      *�ǽ����Ų�
           03  RECE200-LASTSRYKA           PIC X(02).
      *�ǽ������ֹ�
           03  RECE200-LASTBTUNUM          PIC X(02).
      *�ǽ��¼��ֹ�
           03  RECE200-LASTBRMNUM          PIC X(08).
      *
      *����ǯ����
           03  RECE200-NYUINYMD            PIC X(08).
      *
      *���ŵ��ؼ���
           03  RECE200-HOSPSBT             PIC X(01).
      *�����ʬ����
           03  RECE200-BYOTOKBNINF         OCCURS  4.
      *�����ʬ
               05  RECE200-BYOTOKBN        PIC X(02).
      *������̾���
           03  RECE200-BYOTOSYUBETUINF     OCCURS 31.
      *�����ֹ�
               05  RECE200-BYOTONUM        PIC X(02).
      *�������
               05  RECE200-BYOTOSYUBETU    PIC X(02).
      *
      *���Ų���ơ��֥�
           03  RECE200-DAY-AREA.
               05  RECE200-DAY             PIC 9(03)
                                           OCCURS  31.
      *
      *�������ơ��֥����
           03  RECE200-JNISSUTBLINF        OCCURS  31.
      *�������ơ��֥�
               05  RECE200-JNISSUTBL       PIC X(01).
      *
      *��������������ե饰
           03  RECE200-DOUJITSU-FLG        PIC 9(01).
      *����������
           03  RECE200-DOUJITSU-AREA.
      *Ʊ��������������ʬ�ˤ��ݸ��ȹ礻
               05  RECE200-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *Ʊ��������������ʬ�ˤο������ϻ��ֶ�ʬ
               05  RECE200-DOUJITSU-SKJKBN
                                          PIC X(01)   OCCURS  31.
      *������ʬ
               05  RECE200-NYUINDAY-TBL.
                   07  RECE200-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *
      *��ǻ��ֶ�ʬ
           03  RECE200-SHOSHINTIME         PIC X(01).
      *�����®���θ����û��ե饰
           03  RECE200-KENTAIKASANFLG      PIC X(01).
      *�����ȥ�����ȥե饰
           03  RECE200-COMCNTFLG           PIC X(01).
      *�¾����Խ��ե饰������������ܻ�Ƴ������
           03  RECE200-BEDSUFLG            PIC X(01).
      *���̤ʴط��ˤ���¾���򤢤�ե饰
           03  RECE200-TAINFLG             PIC X(01).
      *������
           03  RECE200-PRTSEQ              PIC 9(03).
      *̤����ե饰
           03  RECE200-MISEIKYU-FLG        PIC X(02).
      *̤������ʬ
           03  RECE200-TEISEIKBN           PIC X(01).
      *�쥻�ץȼ����������顼����
           03  RECE200-ERR-AREA.
               05  RECE200-ERR-ERRKBN      PIC 9(01).
               05  RECE200-ERR-DAY         PIC 9(02).
               05  RECE200-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE200-ERR-TEKSTYMD    PIC 9(08).
               05  RECE200-ERR-TEKEDYMD    PIC 9(08).
      *
      *Ķ�žɻ��ʼԡ��������Ųû����λ���ե饰
           03  RECE200-CYOJKSNFLG          PIC X(01).
      *
      *�ƿǲû��������˾���
           03  RECE200-NYUSAIINF.
               05  RECE200-NYUSAI-TENSU    PIC 9(08).
      *���������̿��Ŷ�ʬ����
           03  RECE200-NYUDAYINF.
               05  RECE200-NYUDAY-SRYKBN   PIC X(02)  OCCURS 31.
      *
      *��ִǸ�������������������
           03  RECE200-KANGOGENINF.
               05  RECE200-KANGOGEN-SRYCD  PIC X(09)  OCCURS 31.
      *
      *ǥ�إե饰
           03  RECE200-NINPUFLG            PIC 9(01).
      *
      *�����ե饰
           03  RECE200-KENSA-BNPFLG        PIC 9(01).
           03  RECE200-KENSA-NTPROFLG      PIC 9(01).
           03  RECE200-KENSA-ANPFLG        PIC 9(01).
      *
      *��ϥӥ����
           03  RECE200-RIHAINF-AREA.
      *��ϥӥ껻��ơ��֥룲
               05  RECE200-RIHA2-DAY-AREA.
                 06  RECE200-RIHA2-SINDAI-AREA.
                   07  RECE200-RIHA2-SINDAI      PIC 9(01).
                   07  RECE200-RIHA2-SINYMD-OCC1.
                     08  RECE200-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-SINYMD-OCC2.
                     08  RECE200-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-KOKYUKI-AREA.
                   07  RECE200-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE200-RIHA2-KOKYMD-OCC1.
                     08  RECE200-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-KOKYMD-OCC2.
                     08  RECE200-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-NOUKE-AREA.
                   07  RECE200-RIHA2-NOUKE       PIC 9(01).
                   07  RECE200-RIHA2-NOUYMD-OCC1.
                     08  RECE200-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-NOUYMD-OCC2.
                     08  RECE200-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-UNDOKI-AREA.
                   07  RECE200-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE200-RIHA2-UNDYMD-OCC1.
                     08  RECE200-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-UNDYMD-OCC2.
                     08  RECE200-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-NANBYO-AREA.
                   07  RECE200-RIHA2-NANBYO      PIC 9(01).
                   07  RECE200-RIHA2-NANYMD-OCC1.
                     08  RECE200-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-NANYMD-OCC2.
                     08  RECE200-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-SYOGAI-AREA.
                   07  RECE200-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE200-RIHA2-SYOYMD-OCC1.
                     08  RECE200-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-SYOYMD-OCC2.
                     08  RECE200-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-SESSYOKU-AREA.
                   07  RECE200-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE200-RIHA2-SESYMD-OCC1.
                     08  RECE200-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-SESYMD-OCC2.
                     08  RECE200-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-HAIYO-AREA.
                   07  RECE200-RIHA2-HAIYO       PIC 9(01).
                   07  RECE200-RIHA2-HAIYMD-OCC1.
                     08  RECE200-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-HAIYMD-OCC2.
                     08  RECE200-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *��ϥӥ�û�����ơ��֥룲
               05  RECE200-RIHA2-DAY9-AREA.
                 06  RECE200-RIHA2-SINDAI9-AREA.
                   07  RECE200-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE200-RIHA2-SIN9YMD-OCC1.
                     08  RECE200-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-SIN9YMD-OCC2.
                     08  RECE200-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-KOKYUKI9-AREA.
                   07  RECE200-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE200-RIHA2-KOK9YMD-OCC1.
                     08  RECE200-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-KOK9YMD-OCC2.
                     08  RECE200-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-HAIYO9-AREA.
                   07  RECE200-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE200-RIHA2-HAI9YMD-OCC1.
                     08  RECE200-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-HAI9YMD-OCC2.
                     08  RECE200-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *��Ǣ��Ω�ٱ�û��γ�����ǽ�λ�����
           03  RECE200-HAIJSKYMD                 PIC X(08).
      *
      *ͽ��
           03  RECE200-YOBIETC-AREA.
               05  RECE200-YOBIETCINF      OCCURS 10.
                   07  RECE200-YOBIETC     PIC 9(09).
      *