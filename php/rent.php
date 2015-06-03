<?php

/**
 * 短租的数据结构
 */
class Data_Rent extends Data_Drive_Mongo
{
    protected $dbName = 'duanzu';
    protected $collection = 'duanzu';
    protected $driveCfg = [];

    // 最大结果数
    private static $max = 640;

    /**
     * 字段
     */
    protected $fields = [
        'cityid',
        'primary',
        'name',
        'loc',
        'tel',
        'aoi',
        'avg_price',
        'avg_rating',
        'src',
        'province',
        'city',
        'area',
        'adcode',
        'id',
        'rentType',
        'business_url',
        'landlord_id',
        'landlord_screenname',
        'photo_url',
        'landlord_pic',
        'x',
        'y',
        'distance',
        'review_count',
        'type'
    ];

    /**
     * 房屋类型
     */
    public static $roomTypes = [
        '1' => '民宅',
        '2' => '公寓',
        '3' => '别墅',
        '4' => '旅馆/宾馆',
        '5' => '客栈',
        '7' => '四合院/宅院',
        '8' => '农家院',
        '9' => '海景房',
        '10' => '主题房',
        '11' => '其他'
    ];

    /**
     * 出租类型
     */
    public static $rentType = [
        '1' => '整租',
        '2' => '单间出租（隔断）',
        '3' => '单间出租',
        '4' => '床位出租',
        '5' => '其他'
    ];

    protected $document = null;

    /**
     * 初始化数据
     * @param {Array} $data 一个order数组
     */
    function __construct($data = null) {
        $this->driveCfg = $this->initCfg();

        parent::__construct();

        if (is_array($data)) {
            $this->setArray($data);
        }
    }

    /**
     * 初始化数据驱动配置
     */
    function initCfg()
    {
        $specKey = 'mongo_hour_order';
        $qconfKey = 'mapso/map/resource_mongo/depth';

        $spec = MapConf::getSpec($qconfKey);

        $username = '';
        $password = '';
        $replicaSet = '';

        if ($spec) {
            list($servers, $port, $username, $password) = explode(':', $spec);
            $servers = [$servers . ':' . $port];
        } else {
            list($type, $replicaSet, $magic, $username, $password) = explode('_', MapConf::getCfg($qconfKey));

            $servers = MapConf::getHosts("dba/mdb/" . implode('_', array($type, $replicaSet, $magic)));
        }

        $options = [];

        if ($username) {
            $options['username'] = $username;
        }

        if ($password) {
            $options['password'] = $password;
        }

        if ($replicaSet) {
            $options['replicaSet'] = $replicaSet;
        }

        return [
            'servers' => $servers,
            'options' => $options
        ];
    }

    function set($key, $value = null)
    {
        if (is_array($key)) {
            return $this->setArray($key);
        }

        $this->document[$key] = $value;
    }

    function setArray($data) {
        foreach ($data as $property_name => $value) {
            if (in_array($property_name, $this->fields)) {
                $this->document[$property_name] = $value;
            }
        }

        return true;
    }

    function __get($property_name)
    {
        if (in_array($property_name, $this->fields)) {
            return $this->document[$property_name];
        } else {
            return NULL;
        }
    }

    function __set($property_name, $value)
    {
        $this->document[$property_name] = $value;
    }

    function findOne()
    {
        $cursor = $this->getCollection();

        return call_user_func_array([$cursor, 'findOne'], func_get_args());
    }

    /**
     * 整理
     */
    public function clean($rows, $x = 0, $y = 0)
    {
        $clean = [];

        if (empty($rows)) {
            return $clean;
        }

        $switch = [
            'commentCount' => 'review_count',
            'roomType' => 'type',
            'district' => 'area',
            'addr' => 'address'
        ];

        foreach ($rows as $k => $r) {
            // 扁平detail
            $r = array_merge($r, $r['src']['xiaozhu']);
            unset($r['src']);

            $r['x'] = $r['loc'][0];
            $r['y'] = $r['loc'][1];

            // 计算距离
            if ($x && $y) {
                $r['distance'] = Lib_Geo::GetDistance($y, $x, $r['y'], $r['x']) * 1000;
            }

            // 处理房屋类型
            if (isset(self::$roomTypes[$r['roomType']])) {
                $r['roomType'] = self::$roomTypes[$r['roomType']];
            } else {
                $r['roomType'] = '其他';
            }

            foreach ($switch as $field => $swi) {
                if (isset($r[$field])) {
                    $r[$swi] = $r[$field];
                    unset($r[$field]);
                }
            }

            // 删除字段
            unset($r['sourceid']);
            unset($r['clz']);
            unset($r['person']);
            unset($r['update_time']);

            $clean[] = $r;
        }

        return $clean;
    }


    public function getCond($x, $y, $cityid)
    {
        if ($x && $y) {
            $cond = [
                'loc' => [
                    '$near' => [
                        '$geometry' => [
                            'type' => 'Point',
                            'coordinates' => [$x, $y]
                        ],
                        '$maxDistance' => 20000
                    ]
                ]
            ];
        } else {
            $cond = [
                'cityid' => strval($cityid)
            ];
        }

        return $cond;
    }

    /**
     * 根据中心点返回
     */
    public function getList($x, $y, $cityid, $page = 1, $pagesize = 10)
    {
        $cond = $this->getCond($x, $y, $cityid);
        $row = $this->find($cond, ['_id' => 0]);
        $offset = ($page - 1) * $pagesize;
        if ($offset) {
            $row->skip($offset);
        }
        $row->limit($pagesize);

        return [
            'data' => $this->clean(iterator_to_array($row), $x, $y),
            'count' => $this->getCount($x, $y, $cityid)
        ];
    }

    public function getCount($x, $y, $cityid)
    {
        $cond = $this->getCond($x, $y, $cityid);        
        $row = $this->find($cond, ['_id' => 1]);
        $row->limit(self::$max);
        $count = count(iterator_to_array($row));

        return $count;
    }
}