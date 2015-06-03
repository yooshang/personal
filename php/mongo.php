<?php

class Data_Drive_Mongo
{
    /**
     * 数据驱动配置
     * servers, username, password
     */
    protected $driveCfg = [];
    protected $readPreference = MongoClient::RP_NEAREST;

    protected $fields = [];

    protected $document = [];

    protected $qconfKey = '';

    // 最大执行时间
    protected $maxTimeMS = 250;

    public function __construct()
    {
        // server port username password readPreference
        $this->driveCfg = $this->initCfg();
    }

    public function initCfg()
    {
        $spec = MapConf::getSpec($this->qconfKey);

        $username = '';
        $password = '';
        $replicaSet = '';

        if ($spec && is_array($spec)) {
            $servers = $spec['servers'];
            $username = $spec['username'];
            $password = $spec['password'];
            $replicaSet = $spec['replicaSet'];
        } else {
            $servers = MapConf::getHosts("dba/mdb/" . $this->qconfKey);
        }

        return [
            'servers' => $servers,
            'options' => [
                'username' => $username,
                'password' => $password,
                'replicaSet' => $replicaSet
            ]
        ];
    }

    public function getServers()
    {
        return $this->driveCfg['servers'];
    }

    public function getOptions()
    {
        return $this->driveCfg['options'];
    }

    public function getFields()
    {
        return $this->fields;
    }

    public function getLink($options = null)
    {
        $options = $options ? $options : $this->getOptions();
        $link = Lib_mongo::applyMongo($this->getServers(), $options);

        return $link;
    }

    /**
     * 获得$db
     * @return {Collection}
     */
    public function getDb($link = null, $dbName = '')
    {
        if (is_null($link)) {
            $link = $this->getLink();
        }

        $dbName = $dbName ? $dbName : $this->dbName;

        return $link->$dbName;
    }

    /**
     * 获得collection
     * @return {Collection}
     */
    public function getCollection($db = null, $collection = '')
    {
        if (is_null($db)) {
            $db = $this->getDb();
        }

        $collection = $collection ? $collection : $this->collection;

        return $db->$collection;
    }

    public function getDocument()
    {
        $document = $this->document;

        if (!isset($document['ctime'])) {
            $document['ctime'] = time();
        }

        if (!isset($document['utime'])) {
            $document['utime'] = time();
        }

        return $this->document;
    }

    public function getSaveCollection()
    {
        $options = $this->getOptions();
        $link = Lib_mongo::applyMongo($this->getServers(), $options);

        $db = $this->getDb($link);
        $collection = $this->getCollection($db);

        return $collection;
    }

    public function save()
    {
        $collection = $this->getSaveCollection();
        $aDocument = $this->getDocument();

        $bool = $collection->save($aDocument);

        if ($bool['ok']) {
            return true;
        }

        return false;
    }

    public function find()
    {
        $cursor = $this->getCollection();

        return call_user_func_array([$cursor, 'find'], func_get_args());        
    }

    public function findOne()
    {
        $cursor = $this->getCollection();

        return call_user_func_array([$cursor, 'findOne'], func_get_args());
    }

    public function getList($cond, $sort = [], $page = 1, $pagesize = 10)
    {
        $row = $this->find($cond, ['_id' => 0]);
        $offset = ($page - 1) * $pagesize;

        if ($offset) {
            $row->skip($offset);
        }

        if (!is_null($pagesize)) {
            $row->limit($pagesize);
        }

        if ($sort) {
            $row->sort($sort);
        }

        $rows = iterator_to_array($row);
        return $rows ? $rows : [];
    }

    /**
     * 该函数直接调用drive提供的collection方法
     */
    public function __call($name, $arguments)
    {
        $cursor = $this->getCollection();

        return call_user_func_array([$cursor, $name], $arguments);
    }

    /**
     * 获得数据
     */
    public function getRes($cursor)
    {
        $cursor->maxTimeMS($this->maxTimeMS);

        $res = iterator_to_array($cursor);

        return $res;
    }
}