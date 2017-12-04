import boto3
from datetime import timedelta
from datetime import datetime
import time
import os

region = os.environ['region2']
ACCESS_KEY = os.environ['AWS_ACCESS_KEY_ID']
SECRET_KEY = os.environ['AWS_SECRET_ACCESS_KEY']
cwWindow = 60*24
client = boto3.client('cloudwatch',region_name=region, aws_access_key_id=ACCESS_KEY, aws_secret_access_key=SECRET_KEY)

def aws_cw(instance):
    response = client.get_metric_statistics(
        Namespace='AWS/ELB',
        MetricName='HealthyHostCount',
        Dimensions=[
            {
                'Name': 'LoadBalancerName',
                'Value': instance
            },
        ],
        StartTime=datetime.utcnow() - timedelta(minutes=cwWindow ),
        EndTime=datetime.utcnow(),
        Period=60,
        Statistics=['Average'],
        Unit='Count'
    )
    dp = response["Datapoints"]
    dic = []
    for i in dp:
        t = i['Timestamp']
        t = time.mktime(t.timetuple())
        dic.append({"time" : t, "count_avg": i["Average"]})
    return dic
